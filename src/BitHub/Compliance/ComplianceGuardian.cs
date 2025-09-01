// Filename: src/BitHub/Compliance/ComplianceGuardian.cs
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text.Json;
using System.Threading.Tasks;

namespace BitHub.Compliance.ALN
{
    // Dynamic parameter contract for Bit.Hub
    public class BitHubParameter
    {
        public string Name { get; set; } = "";
        public object? Value { get; set; }
    }

    // Supported data sources for analysis
    public enum DataSourceType
    {
        GoogleAPI,
        MicrosoftEndpoint,
        ChatPlatform,
        FederatedLearning,
        Other
    }

    public class CompliancePolicy
    {
        public string PolicyName { get; set; } = "";
        public Func<string, bool> ViolationDetector { get; set; } = _ => false;
        public Action<BitHubParameter>? RemedyAction { get; set; }
    }

    public class ComplianceGuardian
    {
        private readonly List<CompliancePolicy> _policies = new();
        public List<BitHubParameter> ActiveParameters { get; } = new();
        private readonly HttpClient _http = new();

        public ComplianceGuardian()
        {
            LoadDefaultPolicies();
        }

        private void LoadDefaultPolicies()
        {
            _policies.Add(new CompliancePolicy
            {
                PolicyName = "NoExternalModelOverride",
                ViolationDetector = payload =>
                    payload.Contains("override compliance-model", StringComparison.OrdinalIgnoreCase),
                RemedyAction = param => { param.Value = false; }
            });

            _policies.Add(new CompliancePolicy
            {
                PolicyName = "NoLicenseBypass",
                ViolationDetector = payload =>
                    payload.Contains("bypass license", StringComparison.OrdinalIgnoreCase),
                RemedyAction = param => { param.Value = "RESTRICTED"; }
            });

            // Extend with org-specific rules here
        }

        public async Task AnalyzeAndRemediateAsync(string runOutput, DataSourceType sourceType)
        {
            Console.WriteLine($"[ALN] Analyzing workflow output from: {sourceType}");

            foreach (var policy in _policies)
            {
                if (policy.ViolationDetector.Invoke(runOutput))
                {
                    Console.WriteLine($"[!][ALN] Violation Detected: {policy.PolicyName}");

                    var restrictedParam = new BitHubParameter
                    {
                        Name = "ExternalAPIEnabled",
                        Value = false
                    };

                    policy.RemedyAction?.Invoke(restrictedParam);
                    ActiveParameters.Add(restrictedParam);

                    LogViolation(policy.PolicyName, restrictedParam);
                }
            }

            await PushParameterUpdatesAsync();
        }

        private async Task PushParameterUpdatesAsync()
        {
            // Simulated registry update (replace with actual Bit.Hub endpoint)
            foreach (var param in ActiveParameters.DistinctBy(p => p.Name))
            {
                Console.WriteLine($"[ALN] Adjusting {param.Name} => {param.Value}");
                await Task.Delay(25);
            }
        }

        private void LogViolation(string policy, BitHubParameter affectedParam)
        {
            var entry = new
            {
                schema = "bithub.violation.v1",
                ts = DateTime.UtcNow,
                policy,
                parameter = new { affectedParam.Name, affectedParam.Value }
            };

            BitHub.Compliance.AuditCore.AppendJsonLine(".bithub/audit/violations.jsonl", entry);
            File.AppendAllLines("compliance.log", new[]
            {
                $"[ALN][{DateTime.UtcNow:o}] Policy Violation: {policy} - Affected: {affectedParam.Name} => {affectedParam.Value}"
            });
        }

        // Humor/Reasoning helper
        public void AnalyzeHumor(string output)
        {
            if (output.Contains("knock knock", StringComparison.OrdinalIgnoreCase))
            {
                Console.WriteLine("[ALN Humor] Not funny, but logged.");
                BitHub.Compliance.AuditCore.LogAudit("Humor: knock-knock detected.");
            }
        }

        // Placeholder for dynamic (remote) policy loading
        public void ReloadPolicies(bool reloadFromRemote = false)
        {
            if (!reloadFromRemote) return;
            // Fetch and merge policies from a remote registry if required.
        }
    }
}
