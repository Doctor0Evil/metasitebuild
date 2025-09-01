// SecureBLEMasterSystem.java
// Unified Master Implementation: BLE + AI + Blockchain + 6G + Trust-Level Auth (hardened)

package com.realityos.mastersecurity;

import android.Manifest;
import android.bluetooth.BluetoothAdapter;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.RequiresApi;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.ContextCompat;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;

import com.realityos.security.*;
import com.realityos.voice.VoiceCommandProcessor;
import com.realityos.cloud.*;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Hardened master orchestrator:
 * - Runtime permission gating for BLE and Voice (Android 12+ and legacy).
 * - Bluetooth enable flow with ActivityResult API.
 * - No secrets in code: wallet is resolved from secure storage or BuildConfig at runtime.
 * - Lifecycle-aware start/stop of monitors (onStart/onStop).
 * - Background-thread offloading for heavy/security operations.
 */
public class SecureBLEMasterSystem extends AppCompatActivity {

    private static final String TAG = "SecureBLEMasterSystem";

    // BLE / System
    private BluetoothAdapter bluetoothAdapter;

    // Subsystems
    private AIBot aiBot;
    private TrustLevelAuth trustLevelAuth;
    private VoiceCommandProcessor voiceProcessor;
    private CloudAIThreatMonitor cloudMonitor;
    private AI6GSecurity ai6GSecurity;
    private BlockchainLogger blockchainLogger;
    private AIBLEFirewall firewall;
    private AIAttackPredictor aiAttackPredictor;
    private AIAnomalyDetection aiAnomalyDetection;
    private AICybersecurityAssistant cyberAssistant;
    private SecureBLEBroadcaster broadcaster;

    // Execution
    private ExecutorService bg;

    // Permissions and enable flows
    private ActivityResultLauncher<String[]> permLauncher;
    private ActivityResultLauncher<Intent> btEnableLauncher;

    // Resolve admin wallet at runtime; do not hardcode secrets
    private String adminEthWallet;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        bg = Executors.newCachedThreadPool();

        bluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
        if (bluetoothAdapter == null) {
            Log.e(TAG, "Device has no Bluetooth adapter; finishing.");
            finish();
            return;
        }

        // Register Activity Result launchers
        registerLaunchers();

        // Request runtime permissions (BLE + Voice), then continue init if granted
        requestRuntimePermissions();
    }

    @Override
    protected void onStart() {
        super.onStart();
        // Start passive monitors when Activity is visible (after init completed)
        if (cloudMonitor != null) cloudMonitor.beginMonitoring();
        if (firewall != null) firewall.activate();
        if (broadcaster != null && bluetoothAdapter.isEnabled()) {
            tryStartBroadcast();
        }
    }

    @Override
    protected void onStop() {
        // Tidy monitors; keep state light when not visible
        if (cloudMonitor != null) cloudMonitor.pauseMonitoring();
        if (broadcaster != null) broadcaster.stopBroadcast();
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        if (bg != null) bg.shutdownNow();
        super.onDestroy();
    }

    // region Init

    private void registerLaunchers() {
        permLauncher = registerForActivityResult(
                new ActivityResultContracts.RequestMultiplePermissions(),
                result -> {
                    boolean allGranted = true;
                    for (Boolean granted : result.values()) {
                        if (granted == null || !granted) {
                            allGranted = false;
                            break;
                        }
                    }
                    if (allGranted) {
                        ensureBluetoothEnabledThenInit();
                    } else {
                        Log.e(TAG, "Required permissions not granted; finishing.");
                        finish();
                    }
                });

        btEnableLauncher = registerForActivityResult(
                new ActivityResultContracts.StartActivityForResult(),
                r -> {
                    if (bluetoothAdapter.isEnabled()) {
                        initSystems();
                    } else {
                        Log.e(TAG, "Bluetooth not enabled by user; finishing.");
                        finish();
                    }
                });
    }

    private void requestRuntimePermissions() {
        String[] perms;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            // Android 12+ requires BLUETOOTH_* runtime permissions
            perms = new String[]{
                    Manifest.permission.BLUETOOTH_SCAN,
                    Manifest.permission.BLUETOOTH_CONNECT,
                    // If you advertise (broadcaster), request ADVERTISE too:
                    Manifest.permission.BLUETOOTH_ADVERTISE,
                    // Voice commands:
                    Manifest.permission.RECORD_AUDIO
            };
        } else {
            // Legacy (<= Android 11): location is required for BLE scanning
            perms = new String[]{
                    Manifest.permission.ACCESS_FINE_LOCATION,
                    Manifest.permission.RECORD_AUDIO
            };
        }

        if (hasAll(perms)) {
            ensureBluetoothEnabledThenInit();
        } else {
            permLauncher.launch(perms);
        }
    }

    private boolean hasAll(String[] perms) {
        for (String p : perms) {
            if (ContextCompat.checkSelfPermission(this, p) != PackageManager.PERMISSION_GRANTED) {
                return false;
            }
        }
        return true;
    }

    private void ensureBluetoothEnabledThenInit() {
        if (!bluetoothAdapter.isEnabled()) {
            Intent enableIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
            btEnableLauncher.launch(enableIntent);
        } else {
            initSystems();
        }
    }

    @MainThread
    private void initSystems() {
        // Resolve admin wallet from secure storage or BuildConfig; no hardcoded secrets
        adminEthWallet = loadAdminWallet();

        // Initialize subsystems
        Set<String> authorizedDevices = new HashSet<>();
        authorizedDevices.add("DEVICE_MAC_1"); // TODO: replace with provisioned list

        aiBot = new AIBot(bluetoothAdapter, authorizedDevices);
        trustLevelAuth = new TrustLevelAuth(this);
        firewall = new AIBLEFirewall();
        ai6GSecurity = new AI6GSecurity();
        blockchainLogger = new BlockchainLogger();
        aiAttackPredictor = new AIAttackPredictor();
        aiAnomalyDetection = new AIAnomalyDetection();
        cyberAssistant = new AICybersecurityAssistant(this);
        cloudMonitor = new CloudAIThreatMonitor();
        broadcaster = new SecureBLEBroadcaster(this);
        voiceProcessor = new VoiceCommandProcessor(this);

        RemoteSecurityDashboard.init();

        // Perform trust-level authentication asynchronously
        bg.submit(() -> {
            try {
                trustLevelAuth.authenticateWallet(adminEthWallet, result -> {
                    if (result != null && result.isTrusted()) {
                        runOnUiThread(() -> {
                            tryStartBroadcast();
                            if (cloudMonitor != null) cloudMonitor.beginMonitoring();
                            if (firewall != null) firewall.activate();
                        });
                    } else {
                        Log.w(TAG, "Trust-level authentication failed or untrusted.");
                    }
                });
            } catch (Exception e) {
                Log.e(TAG, "Trust-level auth error", e);
            }
        });
    }

    private void tryStartBroadcast() {
        try {
            if (broadcaster != null) {
                broadcaster.startBroadcast();
            }
        } catch (SecurityException se) {
            Log.e(TAG, "Missing BLUETOOTH_ADVERTISE permission for broadcast", se);
        } catch (Exception e) {
            Log.e(TAG, "Broadcast start failed", e);
        }
    }

    // endregion

    // region Public API

    public void analyzeTraffic(@NonNull String type, @NonNull String ip) {
        bg.submit(() -> {
            try {
                ai6GSecurity.analyze6GTraffic(type, ip);
                aiAnomalyDetection.detect(ip);
                aiAttackPredictor.predict(ip);
            } catch (Exception e) {
                Log.e(TAG, "analyzeTraffic error", e);
            }
        });
    }

    public void respondToThreat(@NonNull String ip) {
        bg.submit(() -> {
            try {
                firewall.blockIp(ip);
                blockchainLogger.logThreat(ip);
                cyberAssistant.respondToIncident(ip);
            } catch (Exception e) {
                Log.e(TAG, "respondToThreat error", e);
            }
        });
    }

    // endregion

    // region Secrets & config

    /**
     * Load admin wallet address at runtime.
     * Prefer EncryptedSharedPreferences/RemoteConfig over code constants.
     */
    private String loadAdminWallet() {
        try {
            String k = SecureConfig.loadAdminWallet(this); // your secure loader (Keystore-backed)
            if (k != null && !k.isEmpty()) return k;
        } catch (Exception ignored) {}
        // Fallback to BuildConfig if injected at build time (non-secret public address)
        try {
            return BuildConfig.ADMIN_ETH_WALLET;
        } catch (Throwable t) {
            Log.w(TAG, "ADMIN_ETH_WALLET not set in BuildConfig; using placeholder.");
            return "0x0000000000000000000000000000000000000000";
        }
    }

    // endregion
}
