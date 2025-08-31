using System.Text.Json;
using BitHub.PolicyHarness.Models;

namespace BitHub.PolicyHarness.Services;

public class PolicyLoader
{
    private readonly string _root;
    public PolicyLoader(string root) => _root = root;

    public Policies LoadAll()
    {
        return new Policies
        {
            GitEnforcement = TryRead(Path.Combine(_root, ".gitenforcement")),
            GitComply = TryRead(Path.Combine(_root, ".gitcomply")),
            ThresholdsJson = TryRead(Path.Combine(_root, ".bit/Central_Data_Bank/thresholds.json")),
            HrCoreJson = TryRead(Path.Combine(_root, ".bit/Central_Data_Bank/hr_core.json")),
            PersonalityMatrixJson = TryRead(Path.Combine(_root, ".bit/Central_Data_Bank/personality_matrix.json")),
            TokenJson = TryRead(Path.Combine(_root, ".bit/tokens/runner_bitcoin_token.json"))
        };
    }

    public void ScaffoldIfMissing()
    {
        EnsureFile("TERMS-OF-SERVICE.md",
            "# Bit.Hub Terms\nExecution implies acceptance. Governed by .gitcomply, .gitenforcement, config.bit.create.\n");
        Directory.CreateDirectory(Path.Combine(_root, ".bit", "audit"));
    }

    private static string? TryRead(string path) =>
        File.Exists(path) ? File.ReadAllText(path) : null;

    private void EnsureFile(string rel, string content)
    {
        var path = Path.Combine(_root, rel);
        if (!File.Exists(path))
            File.WriteAllText(path, content);
    }
}

public class Policies
{
    public string? GitEnforcement { get; set; }
    public string? GitComply { get; set; }
    public string? ThresholdsJson { get; set; }
    public string? HrCoreJson { get; set; }
    public string? PersonalityMatrixJson { get; set; }
    public string? TokenJson { get; set; }
}
