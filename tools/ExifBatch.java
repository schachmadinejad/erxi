import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.ByteArrayOutputStream;

/**
 * Batch-Wrapper fuer EXIficientCMD: Liest Befehle von stdin, verarbeitet sie
 * in derselben JVM. Spart ~200ms JVM-Startup pro Aufruf.
 *
 * Protokoll:
 *   Eingabe: Eine Zeile pro Befehl, gleiche Args wie EXIficientCMD
 *            z.B.: -encode -i input.xml -o output.exi -schema foo.xsd
 *   Ausgabe: "OK" oder "ERROR: message" pro Befehl
 *
 * Beenden: stdin schliessen oder leere Zeile.
 */
public class ExifBatch {
    private static class ExitTrappedException extends SecurityException {
        final int status;
        ExitTrappedException(int status) {
            super("System.exit(" + status + ")");
            this.status = status;
        }
    }

    private static void forbidSystemExitCall() {
        try {
            System.setSecurityManager(new SecurityManager() {
                @Override
                public void checkPermission(java.security.Permission perm) {
                    // allow
                }
                @Override
                public void checkPermission(java.security.Permission perm, Object context) {
                    // allow
                }
                @Override
                public void checkExit(int status) {
                    throw new ExitTrappedException(status);
                }
            });
        } catch (Throwable t) {
            // SecurityManager disabled (Java 17+). Continue without exit trap.
            System.err.println("WARN: SecurityManager not available: " + t.getClass().getSimpleName());
        }
    }

    public static void main(String[] args) throws Exception {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));

        // Capture stdout/stderr from EXIficientCMD
        PrintStream origOut = System.out;
        PrintStream origErr = System.err;
        String line;
        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty()) break;

            String[] cmdArgs = line.split("\\s+");

            // Redirect stdout/stderr to capture EXIficientCMD output
            ByteArrayOutputStream capturedOut = new ByteArrayOutputStream();
            ByteArrayOutputStream capturedErr = new ByteArrayOutputStream();
            System.setOut(new PrintStream(capturedOut));
            System.setErr(new PrintStream(capturedErr));

            try {
                com.siemens.ct.exi.main.cmd.EXIficientCMD.main(cmdArgs);

                String errStr = capturedErr.toString().trim();
                if (!errStr.isEmpty() && errStr.contains("Error")) {
                    System.setOut(origOut);
                    System.setErr(origErr);
                    origOut.println("ERROR: " + errStr.replace("\n", " "));
                } else {
                    System.setOut(origOut);
                    System.setErr(origErr);
                    origOut.println("OK");
                }
            } catch (ExitTrappedException e) {
                String errStr = capturedErr.toString().trim();
                if (errStr.isEmpty()) {
                    errStr = "System.exit(" + e.status + ")";
                }
                System.setOut(origOut);
                System.setErr(origErr);
                origOut.println("ERROR: " + errStr.replace("\n", " "));
            } catch (Throwable e) {
                // Throwable faengt auch OutOfMemoryError etc.
                System.setOut(origOut);
                System.setErr(origErr);
                origOut.println("ERROR: " + e.getClass().getSimpleName() + ": " + e.getMessage());
            }
            origOut.flush();
        }
    }
}
