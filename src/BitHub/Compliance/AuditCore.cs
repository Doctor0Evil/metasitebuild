// Filename: src/BitHub/Compliance/AuditCore.cs
using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;

namespace BitHub.Compliance
{
    public static class AuditCore
    {
        public static string Owner = "Jacob Scott Farmer";
        public static string[] Contributors = { "Perplexity AI Team", "Bit.Hub Core", "Community" };
        public static string Sovereignty = "BIT_HUB_WALL_UNBREAKABLE";

        public static bool VerifySovereignty() =>
            Sovereignty == "BIT_HUB_WALL_UNBREAKABLE";

        public static void DynamicAdjust(string aiResult, string coreMetric)
        {
            EnsureDir(".bithub/audit");
            var record = new
            {
                schema = "bithub.adjust.v1",
                aiResult,
                coreMetric,
                time = DateTime.UtcNow
            };
            AppendJsonLine(".bithub/audit/adjust-log.jsonl", record);
        }

        public static void LogAudit(string submission)
        {
            EnsureDir(".bithub/audit");
            using var sha = SHA256.Create();
            var hash = BitConverter.ToString(sha.ComputeHash(Encoding.UTF8.GetBytes(submission))).Replace("-", "");
            var line = $"{hash} :: {DateTime.UtcNow:o}: {submission}";
            File.AppendAllText(".bithub/audit/full.log", line + Environment.NewLine, Encoding.UTF8);
        }

        public static void AppendJsonLine(string filePath, object value)
        {
            EnsureDir(Path.GetDirectoryName(filePath)!);
            var json = JsonSerializer.Serialize(value, new JsonSerializerOptions { WriteIndented = false });
            File.AppendAllText(filePath, json + Environment.NewLine, Encoding.UTF8);
        }

        public static void EnsureDir(string? path)
        {
            if (string.IsNullOrWhiteSpace(path)) return;
            Directory.CreateDirectory(path);
        }
    }
}
