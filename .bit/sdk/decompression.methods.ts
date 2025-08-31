import * as fs from "fs";
import * as zlib from "zlib";
import * as tar from "tar";
import { createGunzip } from "zlib";

export async function decompressFileALN(
    input: string,
    output: string,
    method: "zlib"|"gzip"|"bz2"|"zip"|"lzma"|"tar",
    railguard: boolean = true
) {
    const maxSizeMB = railguard ? 120 : 500;
    if (fs.statSync(input).size / (1024*1024) > maxSizeMB)
        throw new Error("file too big for current runner railguard");

    switch (method) {
        case "gzip":
        case "zlib":
            fs.createReadStream(input)
                .pipe(createGunzip())
                .pipe(fs.createWriteStream(output));
            break;
        case "tar":
            await tar.x({ file: input, C: output });
            break;
        // Add zip, bz2, lzma, etc. as needed
        default:
            throw new Error("unsupported method: " + method);
    }
    // Ledger audit (you can append to .bithub/ledger/runner.log)
    fs.appendFileSync(".bithub/ledger/runner.log",
      JSON.stringify({ts: new Date().toISOString(), method, src: input, dst: output}) + "\n"
    );
}
