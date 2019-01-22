Benchee.run(
  %{
    "part_one" => fn -> Aoc.part_one() end,
    "part_two" => fn -> Aoc.part_two() end
  },
  memory_time: 2,
  formatters: [
    Benchee.Formatters.Console,
    Benchee.Formatters.HTML
  ],
  formatter_options: [
    console: [extended_statistics: true],
    html: [file: "/tmp/aoc/benchmark.html"]
  ]
)

{:ok, p1_result} = Aoc.part_one()
{:ok, p2_result} = Aoc.part_two()
IO.puts("\nResults:\n")
IO.puts("part_one: #{p1_result}")
IO.puts("part_two: #{p2_result}")
