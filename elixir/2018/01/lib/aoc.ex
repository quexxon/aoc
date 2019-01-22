defmodule Aoc do
  @input Path.expand(Path.join(__DIR__, "../input.txt"))

  def part_one do
    File.open(@input, [], fn pid ->
      IO.stream(pid, :line)
      |> Stream.map(&parse_int/1)
      |> Enum.sum()
    end)
  end

  def part_two do
    File.open(@input, [], fn pid ->
      IO.stream(pid, :line)
      |> Enum.map(&parse_int/1)
      |> Stream.cycle()
      |> Enum.reduce_while({0, MapSet.new()}, fn n, {sum, previous_sums} ->
        sum = sum + n

        if MapSet.member?(previous_sums, sum) do
          {:halt, sum}
        else
          {:cont, {sum, MapSet.put(previous_sums, sum)}}
        end
      end)
    end)
  end

  defp parse_int(string) do
    string |> String.trim() |> String.to_integer()
  end
end
