namespace BitHub.PolicyHarness.Services;

public class SummaryWriter
{
    private readonly string? _path;
    private readonly List<string> _buf = new();
    public SummaryWriter(string? path) => _path = path;
    public void Header(string h) => _buf.Add($"## {h}");
    public void Section(string h, IEnumerable<string> lines)
    {
        _buf.Add($"### {h}");
        foreach (var l in lines) _buf.Add(l);
    }
    public void Line(string l) => _buf.Add(l);
    public void Flush()
    {
        if (string.IsNullOrWhiteSpace(_path)) return;
        Directory.CreateDirectory(Path.GetDirectoryName(_path)!);
        File.AppendAllLines(_path!, _buf);
        _buf.Clear();
    }
}
