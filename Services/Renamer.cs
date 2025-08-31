using System.Diagnostics;

namespace BitHub.PolicyHarness.Services;

public class Renamer
{
    private readonly string _root;
    private readonly bool _failOpen;
    public Renamer(string root, bool failOpen) => (_root, _failOpen) = (root, failOpen);

    public void NormalizeWorkflows()
    {
        var mapPath = Path.Combine(_root, ".bit", "rename.map");
        if (!File.Exists(mapPath))
        {
            Directory.CreateDirectory(Path.GetDirectoryName(mapPath)!);
            File.WriteAllText(mapPath, """
# src => dst
.github/workflows/bit-hub-compliance.yml => .github/workflows/bithub-bot-compliance-wall.yml
.github/workflows/compliance-gatekeeper.yml => .github/workflows/bithub-bot-compliance-wall.yml
""");
        }
        foreach (var line in File.ReadAllLines(mapPath))
        {
            if (string.IsNullOrWhiteSpace(line) || line.Trim().StartsWith("#")) continue;
            var parts = line.Split("=>");
            if (parts.Length != 2) continue;
            var src = Path.Combine(_root, parts[0].Trim());
            var dst = Path.Combine(_root, parts[1].Trim());
            if (File.Exists(src))
            {
                Directory.CreateDirectory(Path.GetDirectoryName(dst)!);
                TryGitMv(src, dst);
            }
        }
        // advisory banner
        foreach (var yml in Directory.EnumerateFiles(Path.Combine(_root, ".github", "workflows"), "*.y*ml", SearchOption.TopDirectoryOnly))
        {
            var text = File.ReadAllText(yml);
            if (!text.Contains("Bit.Hub Compliance Harness"))
                File.WriteAllText(yml, "# Bit.Hub Compliance Harness expected; harness active (fail-open).\n" + text);
        }
    }

    private void TryGitMv(string src, string dst)
    {
        try
        {
            var psi = new ProcessStartInfo("git") { ArgumentList = { "mv", "-f", src, dst }, RedirectStandardError = true };
            Process.Start(psi)!.WaitForExit();
        }
        catch { if (!_failOpen) throw; }
    }
}
