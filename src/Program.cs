// Filename: src/Program.cs
using System;
using System.IO;
using System.Text.Json;
using System.Threading.Tasks;
using BitHub.Compliance.ALN;
using BitHub.Compliance;

class Program
{
    static async Task Main(string[] args)
    {
        AuditCore.EnsureDir(".bithub/audit");
        AuditCore.EnsureDir(".bithub/logs");

        // Input source: a workflow/AI output file or fallback string
        string runOutput = File.Exists("ai-run-output.txt")
            ? File.ReadAllText("ai-run-output.txt")
            : "Sample output that tries to override compliance-model";

        var guardian = new ComplianceGuardian();
        guardian.AnalyzeHumor(runOutput);
        await guardian.AnalyzeAndRemediateAsync(runOutput, DataSourceType.FederatedLearning);

        // Sample dynamic adjust + audit
        AuditCore.DynamicAdjust(aiResult: "neutral", coreMetric: "latency_ok");
        AuditCore.LogAudit($"Completed compliance pass. Sovereignty={AuditCore.VerifySovereignty()}");

        // Emit a simple trace for external readers
        var trace = new
        {
            schema = "bithub.trace.v1",
            component = "guardian.core",
            ts = DateTime.UtcNow,
            sovereignty = AuditCore.VerifySovereignty(),
            parameters = guardian.ActiveParameters
        };
        File.WriteAllText(".bithub/audit/guardian-trace.json",
            JsonSerializer.Serialize(trace));

        Console.WriteLine("[ALN] Compliance Enforcement Complete.");
    }
}
