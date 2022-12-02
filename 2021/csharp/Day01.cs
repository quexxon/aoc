public class Day01
{
    public uint[] Input { get; }

    public Day01(string inputPath)
    {
        if (!File.Exists(inputPath))
        {
            throw new Exception("File doesn't exist");
        }

        Input = File.ReadLines(inputPath).Select(line => UInt32.Parse(line)).ToArray();
    }

    public uint Part1()
    {
        uint descent = 0;
        uint previousDepth = Input[0];

        uint depth;
        for (var i = 1; i < Input.Length; i++)
        {
            if ((depth = Input[i]) > previousDepth) descent++;
            previousDepth = depth;
        }

        return descent;
    }

    public uint Part2()
    {
        uint descent = 0;
        uint[] previousDepths = { Input[0], Input[1], Input[2] };

        uint depth, shared, group, prevGroup;
        for (var i = 3; i < Input.Length; i++)
        {
            depth = Input[i];
            shared = previousDepths[1] + previousDepths[2];
            prevGroup = shared + previousDepths[0];
            group = shared + depth;
            if (group > prevGroup) descent++;
            previousDepths[0] = previousDepths[1];
            previousDepths[1] = previousDepths[2];
            previousDepths[2] = depth;
        }

        return descent;
    }
}
