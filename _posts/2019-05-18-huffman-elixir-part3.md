---
layout: post
title: Compressing Text Files With Elixir (Part 3)
subtitle: Decompression
date: 2019-05-18
---

In my [previous blog post](https://nickdichev.com/blog/2019/05/11/huffman-elixir-part2) we saw how to compress input text using the Huffman algorithm. However, we ended with a problem. We have no way to reverse the compression! The compressed data isn't very useful without a way to recover the original data. In this post we will see how we can store some metadata along with the compressed data which will allow us to decompress data!

You can find the full code with comments, type specs, and unit tests on my [Github](https://github.com/nickdichev/huffman-elixir)!

Let's get started!

## Compression Header

As previously mentioned, we need to store some metadata about the input data which will allow us to decompress whatever binary data the compression algorithm spits out. There are several approaches we could take. One method could be to store a serialized version of the Huffman tree which could be used to reconstruct the Huffman tree data structure.

I was looking for a quick solution, so I did some initial experiments using `:erlang.term_to_binary/2` which serializes Erlang terms into binary data. However, the byte size of the serialized Huffman tree was way too large to effectively reduce the input file size. I settled on serializing the character count map into a binary with the same `:erlang.term_to_binary/2`. Let's create a file `lib/huffman/header.ex` and start the implementation of a module that will create the header data.

```elixir
defmodule Huffman.Header do

  def get_header(char_counts) do
    binary_term = :erlang.term_to_binary(char_counts, compressed: 9)
    term_len = byte_size(binary_term)
    {binary_term, term_len}
  end

  def from_binary(binary) do
    :erlang.binary_to_term(binary)
  end
end
```

We implemented two functions: `get_header/1` and `from_binary/1`. Notice that these functions are just thin wrappers around `:erlang.term_to_binary/1` and `:erlang.binary_to_term/1`. Let's look at `get_header/1` first:

The `get_header/1` function takes a character count map as input and starts by serializing the map to a binary with `:erlang.term_to_binary/1`. We pass an option of `compressed: 9` so we can (hopefully) get back the smallest binary possible. We then find the byte size of the binary with `byte_size/1`. Finally, we return the serialized binary and it's size.

The `from_binary/1` function is not very interesting -- we simply take in some binary input (a serialized character count map) and use `:erlang.binary_to_term/1` to get back the original Erlang term.

Before we look at how we will use the header functions in the main module `Huffman` let's look at a small change we need to make in order to successfully decompress some data!

## End Of File (EOF)

Recall that in the last post we buffered a list of Huffman encoded bitstrings into an iolist of binaries. However, this processes also returned a leftover buffer (which is most likely a bitstring). We took this leftover buffer bitstring and padded it with zeroes into a binary. Let's see the code where we handled that:

```elixir
defmodule Huffman do
  alias Huffman.IOHelper

  def compress(input) do
    ...
    {body, buffer} =
      encodings
      |> compressed_output(input)
      |> Enum.to_list()
      |> IOHelper.buffer_output(<<>>, [])

    eof = IOHelper.pad_bitstring(buffer)

    [buffer, eof]
  end
end
```

Currently, if we try to decode the entire data stream (including the "garbage" zeroes) we will end up with more data than we started with. We have no way of knowing where our data stream ends and the zeroes start. We can get around this by adding a special character between where the leftover buffer ends and where the padded zeroes start. When we are decompressing data, we can look for this special character and stop the decompression.

We will be using the largest possible value we can store in one byte (255) as the special character. Now let's look at the changes we have to make in the `Huffman` module.

## Getting Ready For Decompression

We're ready to start making some changes to the `Huffman` module. The changes we need to make are:

1. Generate a Huffman encoding for the EOF character
2. Generate the compression header from the character occurrence map
3. Add the header and EOF to the compressed data

```elixir
defmodule Huffman do
  @header_length 32

  def compress(input) do
    char_counts =
      input
      |> Counter.count()
      |> Map.put(<<255>>, 1)  # Add the EOF character to the character count map

    # Generate the Huffman header that will be used for decompression
    header_task = Task.async(Header, :get_header, [char_counts])

    encodings = get_encodings(char_counts)

    {header, header_num_bytes} = Task.await(header_task)

    # Generate the compressed output from the encodings/input data
    {body, buffer} =
      encodings
      |> compressed_output(input)
      |> Enum.to_list()
      |> IOHelper.buffer_output(<<>>, [])

    eof_encoding = Map.get(encodings, <<255>>)
    eof = IOHelper.pad_bitstring(<<buffer::bitstring, eof_encoding::bitstring>>)

    [<<header_num_bytes::size(@header_length)>>, header, body, eof]
end
```

Let's start from the top of `compress/1`. The first change we made was to add the EOF character to the character count map that is generated from the input data. We do this in order to generate a Huffman encoding for the EOF character. Next, we start up a task to generate the Huffman header in the background. Then, we call `get_encodings/1` on the character count map, just as we did before.

Remember that the encoding map will now contain the EOF character and its encoding. Then, we await on the header task result -- this could likely be moved to the end of the function since the rest of `compress/1` doesn't rely on the header but let's just leave it here for now. Next, we get the buffered body and leftover bitstring just as before.

Now, we get the EOF encoding out of the encoding map, append the encoding to the leftover buffer, and pad that into a binary. We end the function by returning an iolist that contains the number of bytes in the header (stored in 32 bits), the header itself, the compressed body, and EOF. The EOF now contains the end of our data stream, the encoding of the EOF character, and (most likely) some "garbage" zeroes.

Let's see the output of `Huffman.compress/1` with some added annotation:

```elixir
iex(1)> Huffman.compress("go go gophers")
[
  <<0, 0, 0, 53>>, (header byte size)
  <<131, 80, 0, 0, 0, 77, 120, 218, 43, 97, 96, 96, 224, 204, 5, 18, 140, 10,
    137, 76, 96, 58, 53, 145, 17, 76, 167, 39, 50, 131, 233, 12, 40, 63, 31,
    202, 47, 128, 242, 139, 160, 116, 49, 148, 254, 159, 200, 8, ...>>, (header)
  [[[[[], 26], 52], 122], 99], (body)
  <<243, 64>> (EOF)
]
```

## Decompression

Let's implement some functions in the `Huffman` module to handle decompression. We will start with a helper that can handle the iolist output of `Huffman.compress/1`:

```elixir
defmodule Huffman do
  def decompress([header_bytes, header, iodata, eof] = iolist) when is_list(iolist) do
    body_binary = IO.iodata_to_binary(iodata)
    decompress(header_bytes <> header <> body_binary <> eof)
  end
end
```

We simply pattern match on each part of the iolist, flatten the body iolist into a singular binary, then call the "main" decompress function which works on a binary. Let's see that function now:

```elixir
defmodule Huffman do
  ...

  def decompress(<<header_bytes::size(@header_length), rest::binary>>) do
    <<header::bytes-size(header_bytes), body::binary>> = rest

    char_counts = Header.from_binary(header)

    root =
      char_counts
      |> PriorityQueue.from_map()
      |> Tree.from_priority_queue()

    decompressed_output(body, root, root, [])
  end
end
```

This function starts by pattern matching `header_bytes` on the first 32 bits of the compressed binary, and matching `rest` onto the remaining bitstring. Then we separate out the header data and the body data. Remember that the header data is a serialized Erlang term. We pass that binary to our `Header.from_binary/1` wrapper function to get the original character count map. We use that character count map to reconstruct the Huffman tree.

Now that we have managed to reconstruct the Huffman tree we can iterate over the compressed body to reconstruct the input data. The decompression works by consuming one bit of the body at a time. For each bit that we read we will iterate left or right in the Huffman tree until a leaf node is encountered. When we encounter a leaf node we store the character for that leaf node (remember that only the leaf nodes store characters) and continue iterating through the body from the root of the Huffman tree. All of these operations happen in `decompressed_output/4`. First, lets see what each parameter of this function represents.

The first parameter `body` is the compressed data. The second parameter `root` is there so we can "jump" back to the root of the Huffman tree when we encounter a leaf node. the third parameter `root` will be Huffman tree that we iterate over as we attempt to find the leaf nodes. It acts as a "placeholder" during the iteration over the compressed data. Finally, `[]` is an empty iolist which will be used to store the decompressed characters. Let's see the implementation of `decompressed_output/4`.

```elixir
defmodule Huffman do
  ...

  # Final base case of decompression, at a leaf node that is the EOF character.
   defp decompressed_output(_rest, _root, %{left: nil, right: nil, character: <<255>>}, iolist) do
     iolist
   end

   # Append the character of the current leaf node to the iolist.
  defp decompressed_output(rest, root, %{left: nil, right: nil} = node, iolist) do
    decompressed_output(rest, root, root, [iolist, node.character])
  end

  # Consume a 1 off the compressed encoding and go right for the next recursion
  defp decompressed_output(<<1::size(1), rest::bitstring>>, root, node, iolist) do
    decompressed_output(rest, root, node.right, iolist)
  end

  # Consume a 0 off the compressed encoding and go left for the next recursion
  defp decompressed_output(<<0::size(1), rest::bitstring>>, root, node, iolist) do
    decompressed_output(rest, root, node.left, iolist)
  end
end
```

The `decompressed_output/4` function makes extensive use of pattern matching. Let's look at what each case is responsible for:

```elixir
decompressed_output(_rest, _root, %{left: nil, right: nil, character: <<255>>}, iolist)
```

This function will match when we are at the EOF character which was included in `compress/1`. We are able to match on the iteration placeholder tree node and determine we are at this leaf node when `:left` and `:right` are nil (remember, leaf nodes have no children) and the `:character` the current leaf node is holding is the EOF character. For this case, we simply return the iolist that has been built up over the decompression. This ends the recursion.

```elixir
decompressed_output(rest, root, %{left: nil, right: nil} = node, iolist)
```

This function will match when we are at some non-EOF leaf node. Just like previous case, we know we are at a leaf node when the `:left` and `:right` children of the iteration placeholder tree node are nil. In this case, we append the iteration placeholder tree node's character to the iolist. This is how we actually rebuild the input data.

```elixir
decompressed_output(<<1::size(1), rest::bitstring>>, root, node, iolist)
decompressed_output(<<0::size(1), rest::bitstring>>, root, node, iolist)
```

For these cases, we pattern match on the first bit of the compressed data. For this case, we "consume" this character by either iterating to the left or right of the current `node`. This is the same process we used when we were actually finding the character encodings during the inorder traversal of the Huffman tree.

We finally have all the pieces put together, let's see an example:

```elixir
iex(1)> Huffman.compress("go go gophers") |> Huffman.decompress() |> IO.iodata_to_binary()
"go go gophers"
```

## File Output

We can add some helper functions that will let us compress and decompress files:

```elixir
defmodule Huffman do

  def compress_file(filename) do
    compressed_data =
      filename
      |> File.stream!()
      |> compress()

    File.write!(filename <> ".hf", compressed_data)
  end

  def decompress_file(filename) do
    decompressed_data =
      filename
      |> File.read!()
      |> decompress()

    File.write!(filename <> ".orig", decompressed_data)
  end
end
```

## Analysis

Let's do a very brief analysis of our algorithm. One thing to consider is that there is a decent amount of overhead in the header data. This means that for smaller inputs, we will probably end up increasing the size of the data (the opposite of what we're trying to do!). Let's see an example of this:

```elixir
iex(1)> [bytes, header, body, eof] = Huffman.compress("go go gophers")
[
  <<0, 0, 0, 53>>,
  <<131, 80, 0, 0, 0, 77, 120, 218, 43, 97, 96, 96, 224, 204, 5, 18, 140, 10,
    137, 76, 96, 58, 53, 145, 17, 76, 167, 39, 50, 131, 233, 12, 40, 63, 31,
    202, 47, 128, 242, 139, 160, 116, 49, 148, 254, 159, 200, 8, ...>>,
  [[[[[], 26], 52], 122], 99],
  <<243, 64>>
]
iex(2)> bytes <> header <> IO.iodata_to_binary(body) <> eof |> byte_size()
63
iex(3)> "go go gophers" |> byte_size()
13
```

Not looking good, but this is expected. Let's try compressing a [larger file](https://github.com/nickdichev/huffman-elixir/blob/master/test_data/hamlet).

```elixir
iex(1)> Huffman.compress_file("test_data/hamlet")
:ok
iex(2)> Huffman.decompress_file("test_data/hamlet.hf")
:ok
```

And now let's compare the size of the files and compare the original and output files:

```bash
ls -lh test_data/hamlet*
-rw-r--r--  1 ndichev  688605420   175K May  1 23:04 test_data/hamlet
-rw-r--r--  1 ndichev  688605420   102K May 18 15:28 test_data/hamlet.hf
-rw-r--r--  1 ndichev  688605420   175K May 18 15:28 test_data/hamlet.hf.orig

diff test_data/hamlet test_data/hamlet.hf.orig
(no output!)
```

We managed to save 73 kilobytes with our compression and managed to fully recover the input data!

## Conclusion

We've managed to complete the entire implementation of the Huffman algorithm! We are able to compress input data, either from an arbitrary binary or an input file, and recover the original input text by decompressing the program's output! There is some room for improvement, however. There might be more space savings possible by using a more efficient compression header!

Let me know if you have any feedback regarding this blog post series. Thanks for making it all the way through!