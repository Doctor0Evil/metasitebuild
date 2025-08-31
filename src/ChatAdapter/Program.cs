using System.Diagnostics;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;

string repoRoot = Directory.GetCurrentDirectory();
string metaPath = Path.Combine(repoRoot, ".meta", "hyper.parameters.json");
string tokenPath = Path.Combine(repoRoot, ".bit", "tokens", "runner_bitcoin_token.json");
string bitlinksPath = Path.Combine(repoRoot, ".bit", "bitlinks.yml");
string auditDir = Path.Combine(repoRoot, ".bit", "audit");
Directory.CreateDirectory(auditDir);

var argsList = args.ToList();
string? inputPath = GetArg("--input");
string? outputPath = GetArg("--output");
if (inputPath is null || outputPath is null)
{
    Console.Error.WriteLine("Usage: chat-adapter --input payload.json --output reply.json");
    Environment.Exit(0);
}

var meta = File.Exists(metaPath) ? JsonDocument.Parse(File.ReadAllText(metaPath)).RootElement : default;
var chatCfg = Try(meta, "chatNative");
var bitCoinCfg = Try(meta, "bitCoin");
var aiBounds = Try(meta, "aiCompliance").GetProperty("personalityVectorBounds");
string binary = chatCfg.ValueKind != JsonValueKind.Undefined ? chatCfg.GetProperty("binaryPath").GetString() ?? "" : "";
int timeoutSec = chatCfg.ValueKind != JsonValueKind.Undefined ? chatCfg.GetProperty("timeoutSeconds").GetInt32() : 30;
string chatAudit = chatCfg.ValueKind != JsonValueKind.Undefined ? chatCfg.GetProperty("auditLog").GetString() ?? Path.Combine(auditDir, "chat-native.log") : Path.Combine(auditDir, "chat-native.log");

int compscore = 900, profanity = 1, banter = 0;
if (File.Exists(tokenPath))
{
    try
    {
        var tok = JsonDocument.Parse(File.ReadAllText(tokenPath)).RootElement;
        compscore = tok.TryGetProperty("compscore", out var c) && c.TryGetInt32(out var ci) ? ci : compscore;
        profanity = tok.TryGetProperty("profanity", out var p) && p.TryGetInt32(out var pi) ? pi : profanity;
        banter = tok.TryGetProperty("banter", out var b) && b.TryGetInt32(out var bi) ? bi : banter;
    }
    catch { }
}

int compscoreMin = aiBounds.GetProperty("compscore").GetProperty("min").GetInt32();
int profanityMax = aiBounds.GetProperty("profanity").GetProperty("max").GetInt32();
int banterMax = aiBounds.GetProperty("banter").GetProperty("max").GetInt32();

bool withinBounds = compscore >= compscoreMin && profanity <= profanityMax && banter <= banterMax;

// Load payload and resolve bitlinks inside
var payloadJson = JsonDocument.Parse(File.ReadAllText(inputPath)).RootElement;
var payload = JsonSerializer.Deserialize<ChatPayload>(payloadJson, new JsonSerializerOptions{PropertyNameCaseInsensitive = true}) ?? new ChatPayload();

payload.Personality ??= new Personality { Banter = banter, Profanity = profanity, Compscore = compscore };
payload = ResolveBitlinks(payload);

if (!withinBounds)
{
    // Yield and sanitize (fail-open: proceed with adjusted parameters)
    payload.Personality!.Profanity = Math.Min(payload.Personality!.Profanity, profanityMax);
    payload.Sanitized = true;
}

if (!File.Exists(binary))
{
    // Stub response (fail-open)
    var stub = new ChatReply
    {
        Output = "[stub] Chat.native.x86 not found; compliance preserved; proceeding fail-open.",
        Meta = new() { Sanitized = payload.Sanitized, Personality = payload.Personality }
    };
    File.WriteAllText(outputPath, JsonSerializer.Serialize(stub, new JsonSerializerOptions{WriteIndented=true}));
    Audit($"stub reply (binary missing) compscore={compscore} profanity={profanity} banter={banter}");
    Environment.Exit(0);
}

// Invoke native binary
try
{
    var psi = new ProcessStartInfo(binary)
    {
        RedirectStandardInput = true,
        RedirectStandardOutput = true,
        RedirectStandardError = true,
        UseShellExecute = false
    };
    using var proc = Process.Start(psi)!;
    using var stdin = new StreamWriter(proc.StandardInput.BaseStream, new UTF8Encoding(false));
    using var stdout = proc.StandardOutput;
    using var stderr = proc.StandardError;

    // Send JSON payload to stdin for maximum compatibility
    await stdin.WriteAsync(JsonSerializer.Serialize(payload));
    await stdin.FlushAsync();
    stdin.Close();

    using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(timeoutSec));
    string raw = await stdout.ReadToEndAsync(cts.Token);
    proc.WaitForExit();

    // Pass through, but attach compliance meta
    var reply = new ChatReply
    {
        Output = string.IsNullOrWhiteSpace(raw) ? "" : raw.Trim(),
        Meta = new() { Sanitized = payload.Sanitized, Personality = payload.Personality }
    };
    File.WriteAllText(outputPath, JsonSerializer.Serialize(reply, new JsonSerializerOptions{WriteIndented=true}));
    Audit($"ok reply bytes={raw.Length} compscore={compscore} profanity={profanity} banter={banter}");
}
catch (Exception ex)
{
    var err = new ChatReply
    {
        Output = "",
        Error = $"adapter_error: {ex.Message}",
        Meta = new() { Sanitized = payload.Sanitized, Personality = payload.Personality }
    };
    File.WriteAllText(outputPath, JsonSerializer.Serialize(err, new JsonSerializerOptions{WriteIndented=true}));
    Audit($"error {ex.Message}");
}

void Audit(string line)
{
    File.AppendAllText(chatAudit, $"{DateTime.UtcNow:o} {line}\n");
}

string? GetArg(string name)
{
    var i = argsList.IndexOf(name);
    return i >= 0 && i + 1 < argsList.Count ? argsList[i + 1] : null;
}

static JsonElement Try(JsonElement root, string key) =>
    root.ValueKind == JsonValueKind.Undefined || !root.TryGetProperty(key, out var v) ? default : v;

ChatPayload ResolveBitlinks(ChatPayload p)
{
    // simplistic resolver for fields that are likely to contain bitlinks
    p.Context = Resolve(p.Context);
    p.Input = Resolve(p.Input);
    return p;

    string Resolve(string? s)
    {
        if (string.IsNullOrWhiteSpace(s) || !s.StartsWith("bitlink://")) return s ?? "";
        try
        {
            // naive YAML parse for manifest lines: id: target
            if (!File.Exists(bitlinksPath)) return s!;
            var map = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
            foreach (var line in File.ReadAllLines(bitlinksPath))
            {
                if (string.IsNullOrWhiteSpace(line) || line.TrimStart().StartsWith("#")) continue;
                // links:
                //   id:
                //     target: path
                if (line.Trim().EndsWith(":")) continue;
                if (line.Contains("target:"))
                {
                    var parts = line.Trim().Split("target:");
                    var target = parts[1].Trim();
                    // previous non-empty key is assumed ID (lightweight; replace with real YAML in production)
                }
            }
        }
        catch { }
        // fallback: strip scheme
        return s.Replace("bitlink://", "");
    }
}

record Personality
{
    [JsonPropertyName("banter")] public int Banter { get; set; }
    [JsonPropertyName("profanity")] public int Profanity { get; set; }
    [JsonPropertyName("compscore")] public int Compscore { get; set; }
}

record ChatPayload
{
    [JsonPropertyName("context")] public string? Context { get; set; }
    [JsonPropertyName("input")] public string? Input { get; set; }
    [JsonPropertyName("personality")] public Personality? Personality { get; set; }
    [JsonPropertyName("compliance")] public string? Compliance { get; set; }
    [JsonPropertyName("sanitized")] public bool Sanitized { get; set; }
}

record ChatReply
{
    [JsonPropertyName("output")] public string? Output { get; set; }
    [JsonPropertyName("error")] public string? Error { get; set; }
    [JsonPropertyName("meta")] public ReplyMeta? Meta { get; set; }
}

record ReplyMeta
{
    [JsonPropertyName("sanitized")] public bool Sanitized { get; set; }
    [JsonPropertyName("personality")] public Personality? Personality { get; set; }
}
