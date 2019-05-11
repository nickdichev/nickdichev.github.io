---
layout: post
title: Compressing Text Files With Elixir (Part 2)
subtitle: Compression
date: 2019-05-11
---

My [previous blog post](https://nickdichev.com/blog/2019/05/04/huffman-elixir-part1) introduced to the Huffman algorithim and the data structures used to implement it. Be sure to check out that post if you missed it!

In this post we will be exploring how to leverage these data structures and some interesting feature of Elixir to complete the implementation of the Huffman algorithm. The features we will be using are: binary pattern matching, iolists, streams, and recursion.

You can find the full source of the program with comments, typespecs, and unit tests on [Github](https://github.com/nickdichev/huffman-elixir).

Let's get started!

## Counting Characters

As you may recall, the Huffman algorithm compresses text by assigning shorter encodings to characters that occur more frequently in some input text. Let's create a file `lib/huffman/counter.ex` and implement a character counting module.

We will be implementing the function `count/1` which takes a list of binaries and returns a map of `character => count` entries. Let's do some initial exploration on how we can accomplish this in an `iex` shell:

```elixir
iex(1)> String.split("go go gophers", "")
["", "g", "o", " ", "g", "o", " ", "g", "o", "p", "h", "e", "r", "s", ""]
```

We can use `String.split/2` to split the input into graphemes. However, there's a problem! We have some "garbage" strings surrounding the characters that we care about. Let's get rid of them with `Enum.filter/2`:

```elixir
iex(1)> String.split("go go gophers", "") |> Enum.filter(fn x -> x != "" end)
["g", "o", " ", "g", "o", " ", "g", "o", "p", "h", "e", "r", "s"]
```

Looking good! Now, we're ready to count all of these characters. Let's start our implementation of the `Huffman.Counter` module:

```elixir
defmodule Huffman.Counter do
  def count_helper([], acc), do: acc

  def count_helper([head | tail], acc) do
    acc = Map.update(acc, head, 1, &(&1 + 1))
    count_helper(tail, acc)
  end
end
```

The function `count_helper/2` takes a list and and an accumulator (in our case, a map) as parameters. Notice that the function is recursive. In `count_helper([], acc)` we have finished consuming characters from the input list and we can return the accumulator. In the other version of `count_helper/2` we match on the `head` and `tail` of the list (the first element and remaining list, respectively), update the count of the `head` character (or use 1 as a default) using `Map.update/4`, then continue the recursion using the remaining list. Let's implement a function which will call our helper:

```elixir
defmodule Huffman.Counter do
  ...
  def count([]), do: %{}

  def count(binaries) do
    binaries
    |> Stream.map(&String.split(&1, ""))
    |> Stream.map(&Enum.filter(&1, fn x -> x != "" end))
    |> Stream.map(&count_helper(&1, %{}))
  end
end
```

There's a few new things going on here. We use the `Stream` module in order to do lazy enumeration on our input. Using `Stream` lets us create a "recipie" of computation that will be evaluated when required. I will not be covering the intricacies of lazy and eager enumeration, however, there are plenty of resources online if you are curious. What is important to know is that the `Stream` module allows us to enumerate large, potentially infinite, input data without blowing up the program's memory usage. Let's try to count some characters:

```elixir
iex(1)> Huffman.Counter.count(["go", "go"])
#Stream<[
  enum: ["go", "go"],
  funs: [#Function<49.126435914/1 in Stream.map/2>,
   #Function<49.126435914/1 in Stream.map/2>,
   #Function<49.126435914/1 in Stream.map/2>]
]>
```

Whoops! Remember, streams are not evaluated until required. Let's enumerate the `Stream` into a list:

```elixir
iex(3)> Huffman.Counter.count(["go", "go"]) |> Enum.to_list()
[%{"g" => 1, "o" => 1}, %{"g" => 1, "o" => 1}]
```

There's still a problem! We counted the characters for each binary in the input list, however, we need to merge the maps together:

```elixir
defmodule Huffman.Counter do
  ...
  defp merge_maps(x, y), do: Map.merge(x, y, fn _k, v1, v2 -> v2 + v1 end)

  def count(binaries) do
    binaries
    |> Stream.map(&String.split(&1, ""))
    |> Stream.map(&Enum.filter(&1, fn x -> x != "" end))
    |> Stream.map(&count_helper(&1, %{}))
    |> Enum.reduce(&merge_maps(&1, &2))
  end
end
```

Notice that we added a call to `merge_maps/2` inside of `Enum.reduce/2` at the end of `count/1`. This will evaluate the stream for us! Let's see what the output of `count/1` looks like now:

```elixir
iex(1)> Huffman.Counter.count(["go", "go"])
%{"g" => 2, "o" => 2}
```

## Compression

We have most of the pieces we need to compress some data! First, let's take a look at what we need to accomplish:

1. Find the occurance count for each character in the input
2. Generate the Huffman encodings based off the character count
3. Replace each character in the input with the corresponding Huffman encoding
4. Buffer the Huffman encoded bitstrings into binaries

Let's open `lib/huffman.ex` and start implementing `compress/1`:

```elixir
defmodule Huffman do
  def compress(bin) when is_binary(bin), do: compress([bin])

  def compress(input) do
    char_counts = Counter.count(input)
    encodings = get_encodings(char_counts)
  end

  defp get_encodings(char_counts) do
      char_counts
      |> PriorityQueue.from_map()
      |> Tree.from_priority_queue()
      |> Tree.inorder()
  end
end
```

The `compress(bin)` function is a helper so we can call `Huffman.compress("go go gophers")` without having to worry about only passing lists of binaries to `compress/1`.

Notice that `get_encodings/1` is the exact transformation we saw at the end of the previous blog post. Now, let's try to replace each character in the input with the Huffman encodings:

```elixir
defmodule Huffman do
  alias Huffman.IOHelper

  def compress(input) do
    ...

    encodings
    |> compressed_output(input)
    |> Enum.to_list()
  end

  defp compressed_output(encodings, input) do
    input
    |> Stream.map(&String.split(&1, ""))
    |> Stream.map(&Enum.filter(&1, fn x -> x != "" end))
    |> Stream.map(&IOHelper.encode_characters(&1, encodings))
    |> Stream.flat_map(&List.flatten/1)
  end
end
```

We start by doing a similar transformation that we did in `Huffman.Counter.count/1` to split the input and clean up "garbage" strings. Then, we call a helper function `IOHelper.encode_characters/2` to replace the input characters with their Huffman encoding. Finally, we flatten the stream into a singular list. Let's see the implementation of the `IOHelper.encode_characters/2` function:

```elixir
defmodule Huffman.IOHelper do
  def encode_characters(iolist, encodings), do: Enum.map(iolist, &Map.get(encodings, &1))
end
```

This function is pretty simple, we use `Enum.map/2` to replace each character from the `encodings` map we pass as a parameter. However, we have a problem. The result of `compressed_output/2` (when enumerated with `Enum.to_list/1`) will look something like: `[7::size(3), 13::size(4), 2::size(2), ...]`. We have a list of bitstrings!

In Elixir strings, binaries, and bitstrings are all related. All are representations of binary (the "bit" kind of binary) data, however, with certain gaurentees. Strings are the most specific: they are Elixir binaries that are UTF-8 encoded. Binaries, on the other hand, must have a bit length that is a multiple of eight. Finally, bitstrings are the least specific. Bitstrings are any groupings of bits. 

This means that all strings and binaries are also considered bitstrings. For more information, [this](https://medium.com/blackode/playing-with-elixir-binaries-strings-dd01a40039d5) is a pretty comprehensive article with nice graphics to help you understand the difference.

Our problem is that we can only output binaries but we have bitstrings! It doesn't matter if we want to output to `stdout` or to a file, we need to buffer the bitstrings into binaries.

## Buffering Output

Let's start by looking at the changes to `Huffman.compress/1` that we need to make:

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
  end
end
```

We pass the list of bitstrings into a new function `IOHelper.buffer_output/3`. This function is responsible for buffering the bitstrings into binaries. Let's see the implementation:

```elixir
defmodule Huffman.IOHelper do
  ...

  def buffer_output([head | tail], buffer, iolist) do
    buffer = <<buffer::bitstring, head::bitstring>>

    {byte, rest} = completed_byte(buffer)

    {buffer, iolist} =
      if byte != nil and rest != nil do
        {rest, [iolist, byte]}
      else
        {buffer, iolist}
      end

    buffer_output(tail, buffer, iolist)
  end

  def buffer_output([], buffer, iolist), do: {iolist, buffer}

  def completed_byte(<<byte::size(8), rest::bitstring>>), do: {byte, rest}
  def completed_byte(_), do: {nil, nil}
end
```

Let's look at what each parameter of `buffer_output/3` are used for. The first parameter is the input list of bitstrings. The second parameter `buffer` will buffer bitstrings into a binary. When the buffer is full, we will append the completed binary to `iolist`.

The function starts by matching on the `head` (the bitstring on the front of the list) and `tail` (the remainder of the bitstring list). The `head` is appended to the buffer. Then, the we check if the buffer has completed a byte with `completed_byte/1`.

The `completed_byte/1` function makes use of binary pattern matching. We try to match on a byte and catch the remainder in `rest`. We return the completed byte and the leftover bitstring. If there is no completed byte on the front of the buffer we simply return `{nil, nil}`.

If we have completed a byte, we append the completed byte onto `iolist`. The leftover bitstring will be used as the buffer for the next recursive iteration. If no byte was completed, we use the current `buffer` and `iolist` for the next recursive iteration.

Notice that we are using an iolist to join the completed binaries. We don't take the usual penalty of appending to the end of a list by doing this. Because of this behavior, you should always use an iolist when you are building output. However, iolists are considered improper lists. Most of the standard list operations will not function as expected on an iolist. However, this isn't a problem for us since the functions we will use to output the compressed data can handle improper lists.

We end the recursion when we have exhausted the list of bitstrings. However, there might be something (most likely a bistring) left in the buffer. We return `{iolist, buffer}` and will handle the leftover buffer in `Huffman.compress/1`. Let's take a look at that:

```elixir
defmodule Huffman do
  alias Huffman.IOHelper

  ...

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

```elixir
defmodule Huffman.IOHelper do
  ...

  def pad_bitstring(bits) when is_binary(bits), do: bits

  def pad_bitstring(bits) do
    size = bit_size(bits)
    pad_len = (8 * ceil(size / 8)) - size
    <<bits::bitstring, 0::size(pad_len)>>
  end
end
```

There are two cases for `IOHelper.pad_bitstring/1`: the leftover buffer is either a binary or a bitstring. If it is a binary we can simply return it. Otherwise, we pad the bitstring into a binary with 0's in the least significant bits.

Okay! We can finally compress input data! Let's see an example:

```elixir
iex(1)> Huffman.compress("go go gophers")
[[[[[[], 110], 221], 139], 112], <<192>>]
```

Let's check if we actually compressed the data:

```elixir
iex(1)> Huffman.compress("go go gophers") |> IO.iodata_to_binary() |> byte_size()
5

iex(2)> "go go gophers" |> byte_size()
13
```

Looks good! We saved 8 bytes with our implementation!

## Conclusion

At this point we have completed the implementation of the Huffman algorithm. We managed to take some input data, compute Huffman encodings, and replace the input data with the encodings. However, this binary data isn't very useful currently. We have no way to get the original data back!

In the next post we will see how we can store some metadata about the input data that was compressed. This metadata will be used to reconstruct the Huffman tree which can be used to decompress the data returned by `Huffman.compress/1`. Keep your eyes peeled for that post (or check out the Github repo)!