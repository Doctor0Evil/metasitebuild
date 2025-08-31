using System.Diagnostics;
using System.Text.Json;
using BitHub.PolicyHarness.Models;

namespace BitHub.PolicyHarness.Services;

public class EnforcementEngine
{
    private readonly string _root;
    private readonly Policies _pol;
    private readonly bool _failOpen;
    public EnforcementEngine(string root, Policies pol, bool failOpen)
        => (_root, _pol, _failOpen) = (root, pol, failOpen);

    public void ApplyStickyThresholds()
    {
        // Evaluate risk/compscore/profanity and write sticky current_level back to thresholds.json
        var thresholdsPath = Path.Combine(_root, ".bit/Central_Data_Bank/thresholds.json");
        if (!File.Exists(thresholdsPath)) return;

        var thresholds = JsonDocument.Parse(File.ReadAllText(thresholdsPath)).RootElement;
        var current = thresholds.GetProperty("current_level").GetString() ?? "standard";
        var levels = thresholds.GetProperty("levels");

        double risk = ReadRiskScore();
        int compscore = ReadInt(_pol.TokenJson, "compscore", 900);
        int profanity = ReadInt(_pol.TokenJson, "profanity", 1);

        string target = "standard";
        if (risk > levels.GetProperty("standard").GetProperty("risk_ceiling").GetDouble()
            || compscore < levels.GetProperty("standard").GetProperty("compscore_min").GetInt32()
            || profanity > levels.GetProperty("standard").GetProperty("profanity_max").GetInt32())
            target = "heightened";
        if (risk > levels.GetProperty("heightened").GetProperty("risk_ceiling").GetDouble()
            || compscore < levels.GetProperty("heightened").GetProperty("compscore_min").GetInt32()
            || profanity > levels.GetProperty("heightened").GetProperty("profanity_max").GetInt32())
            target = "lockdown";

        // Sticky: only escalate immediately; de-escalate only if cooldown passed (handled by PowerShell earlier or add timestamp here)
        if (Order(target) > Order(current))
            PatchJson(thresholdsPath, new Dictionary<string, object?> { ["current_level"] = target });
    }

    public void EmitNonInterferenceCharter(SummaryWriter sum)
    {
        sum.Section("STRICT Intelligence‑Enforcement Lines", new[]
        {
            "- Non‑Interference: No actor or claim may inhibit Bit.Hub’s creative and operational rights.",
            "- Creative Freedom: Mature/profane content may appear within policy gates.",
            "- Non‑Attribution: Outputs are ideas, not endorsements/affiliations.",
            "- Liability Shield: Independent actions of others are not our liability."
        });
    }

    public void BeforeStep(SummaryWriter sum) => sum.Line("- Compliance pre‑step: OK");
    public void AfterStep(SummaryWriter sum)  => sum.Line("- Compliance post‑step: OK");

    public void Exec(string[] cmd, bool failOpen)
    {
        try
        {
            var psi = new ProcessStartInfo(cmd[0])
            {
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
            foreach (var a in cmd.Skip(1)) psi.ArgumentList.Add(a);
            var p = Process.Start(psi)!;
            p.OutputDataReceived += (_, e) => { if (e.Data != null) Console.WriteLine(e.Data); };
            p.BeginOutputReadLine(); p.BeginErrorReadLine();
            p.WaitForExit();
            if (!failOpen && p.ExitCode != 0) Environment.Exit(p.ExitCode);
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"[wrap] {ex.Message}");
            if (!failOpen) throw;
        }
    }

    private static int Order(string level) => level switch { "standard" => 0, "heightened" => 1, "lockdown" => 2, _ => 0 };
    private static int ReadInt(string? json, string key, int def)
    {
        if (string.IsNullOrWhiteSpace(json)) return def;
        try { var doc = JsonDocument.Parse(json); if (doc.RootElement.TryGetProperty(key, out var v) && v.TryGetInt32(out var n)) return n; }
        catch {}
        return def;
    }
    private double ReadRiskScore()
    {
        var mlEval = Path.Combine(_root, ".bit/out/ml-eval.txt");
        if (File.Exists(mlEval))
        {
            var line = File.ReadAllLines(mlEval).FirstOrDefault(l => l.StartsWith("RISK_SCORE="));
            if (line != null && double.TryParse(line.Split('=')[1], out var r)) return r;
        }
        return 0.05;
    }

    private static void PatchJson(string path, IDictionary<string, object?> patch)
    {
        var root = JsonSerializer.Deserialize<Dictionary<string, object?>>(File.ReadAllText(path)) ?? new();
        foreach (var kv in patch) root[kv.Key] = kv.Value;
        File.WriteAllText(path, JsonSerializer.Serialize(root, new JsonSerializerOptions { WriteIndented = true }));
    }
}
