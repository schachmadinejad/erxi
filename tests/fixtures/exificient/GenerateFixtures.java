/**
 * Generiert alle EXI Test-Fixtures mit Exificient 1.0.4.
 *
 * Verwendung:
 *   cd tests/fixtures/exificient
 *   javac -cp "exificient-core-1.0.4.jar:exificient-1.0.4.jar:exificient-grammars-1.0.4.jar:xercesImpl-2.12.0.jar:." GenerateFixtures.java
 *   java -cp "exificient-core-1.0.4.jar:exificient-1.0.4.jar:exificient-grammars-1.0.4.jar:xercesImpl-2.12.0.jar:." GenerateFixtures
 *
 * WICHTIG: SAXParserFactory.setNamespaceAware(true) ist erforderlich!
 */

import com.siemens.ct.exi.core.EXIFactory;
import com.siemens.ct.exi.core.helpers.DefaultEXIFactory;
import com.siemens.ct.exi.main.api.sax.EXIResult;
import com.siemens.ct.exi.core.CodingMode;
import com.siemens.ct.exi.core.FidelityOptions;
import javax.xml.parsers.SAXParserFactory;
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import org.xml.sax.ext.LexicalHandler;
import org.xml.sax.helpers.DefaultHandler;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.StringReader;
import java.nio.file.Files;

public class GenerateFixtures {
    public static void main(String[] args) throws Exception {
        System.out.println("Generiere EXI Fixtures mit Exificient 1.0.4...\n");

        // Basis XML-Dateien (Standard-Encoding)
        String[] xmlFiles = {
            "simple", "complex", "many_elements", "alphabet",
            "many_attributes", "many_namespaces", "deep_nesting",
            "large_grammar", "long_strings", "repeated_values"
        };

        System.out.println("=== Standard Fixtures ===");
        for (String name : xmlFiles) {
            String xmlPath = "../" + name + ".xml";
            if (!new java.io.File(xmlPath).exists()) {
                System.out.println("SKIP: " + xmlPath + " nicht gefunden");
                continue;
            }
            encode(xmlPath, "../" + name + "_bitpacked.exi", CodingMode.BIT_PACKED);
            encode(xmlPath, "../" + name + "_bytealigned.exi", CodingMode.BYTE_PACKED);
        }

        // Fidelity Options: Comments
        System.out.println("\n=== Fidelity: Comments ===");
        encodeWithFidelity("../comment.xml", "../comment_bitpacked.exi",
            CodingMode.BIT_PACKED, true, false, false, false);
        encodeWithFidelity("../comment.xml", "../comment_bytealigned.exi",
            CodingMode.BYTE_PACKED, true, false, false, false);

        // Fidelity Options: Processing Instructions
        System.out.println("\n=== Fidelity: Processing Instructions ===");
        encodeWithFidelity("../processing_instruction.xml", "../processing_instruction_bitpacked.exi",
            CodingMode.BIT_PACKED, false, true, false, false);
        encodeWithFidelity("../processing_instruction.xml", "../processing_instruction_bytealigned.exi",
            CodingMode.BYTE_PACKED, false, true, false, false);

        // Fidelity Options: DTD
        System.out.println("\n=== Fidelity: DTD ===");
        encodeWithDtd("../doctype.xml", "../doctype_bitpacked.exi", CodingMode.BIT_PACKED);
        encodeWithDtd("../doctype.xml", "../doctype_bytealigned.exi", CodingMode.BYTE_PACKED);

        // Fidelity Options: Prefixes
        System.out.println("\n=== Fidelity: Prefixes ===");
        encodeWithFidelity("../prefixes_preserved.xml", "../prefixes_preserved_bitpacked.exi",
            CodingMode.BIT_PACKED, false, false, false, true);
        encodeWithFidelity("../prefixes_preserved.xml", "../prefixes_preserved_bytealigned.exi",
            CodingMode.BYTE_PACKED, false, false, false, true);

        // Many Namespaces mit Prefixes
        System.out.println("\n=== Many Namespaces mit Prefixes ===");
        encodeWithFidelity("../many_namespaces.xml", "../many_namespaces_prefixes_bytealigned.exi",
            CodingMode.BYTE_PACKED, false, false, false, true);

        // PreCompression
        System.out.println("\n=== PreCompression ===");
        encodePreCompression("../simple.xml", "../simple_precompression.exi");

        // PreCompression mit Prefixes
        System.out.println("\n=== PreCompression mit Prefixes ===");
        encodePreCompressionWithPrefixes("../prefixes_preserved.xml", "../prefixes_precompression.exi");

        // PreCompression mit >100 Values (Spec 9.3: separate Compressed Streams)
        System.out.println("\n=== PreCompression >100 Values ===");
        encodePreCompression("../many_values.xml", "../many_values_precompression.exi");

        // Fragment Mode
        System.out.println("\n=== Fragment Mode ===");
        encodeFragment("../fragment.xml", "../fragment_bitpacked.exi", CodingMode.BIT_PACKED);
        encodeFragment("../fragment.xml", "../fragment_bytealigned.exi", CodingMode.BYTE_PACKED);

        System.out.println("\nFertig!");
    }

    static void encode(String in, String out, CodingMode mode) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(mode);
        f.setFidelityOptions(FidelityOptions.createDefault());

        try (FileOutputStream fos = new FileOutputStream(out);
             FileInputStream fis = new FileInputStream(in)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            // WICHTIG: Namespace-aware muss aktiviert sein!
            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            XMLReader reader = spf.newSAXParser().getXMLReader();
            reader.setContentHandler(r.getHandler());
            reader.parse(new InputSource(fis));
        }

        long size = new java.io.File(out).length();
        String modeName = mode == CodingMode.BIT_PACKED ? "bit-packed" : "byte-aligned";
        System.out.println("  " + out + " (" + size + " bytes, " + modeName + ")");
    }

    static void encodeWithFidelity(String in, String out, CodingMode mode,
                                   boolean comments, boolean pis, boolean dtd,
                                   boolean prefixes) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(mode);

        FidelityOptions fidelity = FidelityOptions.createDefault();
        if (comments) fidelity.setFidelity(FidelityOptions.FEATURE_COMMENT, true);
        if (pis) fidelity.setFidelity(FidelityOptions.FEATURE_PI, true);
        if (dtd) fidelity.setFidelity(FidelityOptions.FEATURE_DTD, true);
        if (prefixes) fidelity.setFidelity(FidelityOptions.FEATURE_PREFIX, true);
        f.setFidelityOptions(fidelity);

        try (FileOutputStream fos = new FileOutputStream(out);
             FileInputStream fis = new FileInputStream(in)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            XMLReader reader = spf.newSAXParser().getXMLReader();
            reader.setContentHandler(r.getHandler());

            // Für Comments und PIs brauchen wir LexicalHandler
            if (comments || pis) {
                try {
                    reader.setProperty("http://xml.org/sax/properties/lexical-handler",
                        r.getHandler());
                } catch (Exception e) {
                    System.out.println("    WARN: LexicalHandler nicht gesetzt: " + e.getMessage());
                }
            }

            reader.parse(new InputSource(fis));
        }

        long size = new java.io.File(out).length();
        String modeName = mode == CodingMode.BIT_PACKED ? "bit-packed" : "byte-aligned";
        System.out.println("  " + out + " (" + size + " bytes, " + modeName + ")");
    }

    static void encodeWithDtd(String in, String out, CodingMode mode) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(mode);

        FidelityOptions fidelity = FidelityOptions.createDefault();
        fidelity.setFidelity(FidelityOptions.FEATURE_DTD, true);
        f.setFidelityOptions(fidelity);

        try (FileOutputStream fos = new FileOutputStream(out);
             FileInputStream fis = new FileInputStream(in)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            // DTD-Validierung deaktivieren (wir haben keine echte DTD-Datei)
            spf.setValidating(false);
            spf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

            XMLReader reader = spf.newSAXParser().getXMLReader();
            reader.setContentHandler(r.getHandler());

            // LexicalHandler für DTD-Events
            try {
                reader.setProperty("http://xml.org/sax/properties/lexical-handler",
                    r.getHandler());
            } catch (Exception e) {
                System.out.println("    WARN: LexicalHandler nicht gesetzt: " + e.getMessage());
            }

            // DeclHandler für DTD-Deklarationen
            try {
                reader.setProperty("http://xml.org/sax/properties/declaration-handler",
                    r.getHandler());
            } catch (Exception e) {
                // Ignorieren - nicht alle Handler unterstützen das
            }

            reader.parse(new InputSource(fis));
        }

        long size = new java.io.File(out).length();
        String modeName = mode == CodingMode.BIT_PACKED ? "bit-packed" : "byte-aligned";
        System.out.println("  " + out + " (" + size + " bytes, " + modeName + ")");
    }

    static void encodePreCompression(String in, String out) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(CodingMode.PRE_COMPRESSION);
        f.setFidelityOptions(FidelityOptions.createDefault());

        try (FileOutputStream fos = new FileOutputStream(out);
             FileInputStream fis = new FileInputStream(in)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            XMLReader reader = spf.newSAXParser().getXMLReader();
            reader.setContentHandler(r.getHandler());
            reader.parse(new InputSource(fis));
        }

        long size = new java.io.File(out).length();
        System.out.println("  " + out + " (" + size + " bytes, pre-compression)");
    }

    static void encodePreCompressionWithPrefixes(String in, String out) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(CodingMode.PRE_COMPRESSION);

        FidelityOptions fidelity = FidelityOptions.createDefault();
        fidelity.setFidelity(FidelityOptions.FEATURE_PREFIX, true);
        f.setFidelityOptions(fidelity);

        try (FileOutputStream fos = new FileOutputStream(out);
             FileInputStream fis = new FileInputStream(in)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            XMLReader reader = spf.newSAXParser().getXMLReader();
            reader.setContentHandler(r.getHandler());
            reader.parse(new InputSource(fis));
        }

        long size = new java.io.File(out).length();
        System.out.println("  " + out + " (" + size + " bytes, pre-compression, prefixes)");
    }

    static void encodeFragment(String in, String out, CodingMode mode) throws Exception {
        EXIFactory f = DefaultEXIFactory.newInstance();
        f.setCodingMode(mode);
        f.setFidelityOptions(FidelityOptions.createDefault());
        f.setFragment(true);

        // Fragment XML wird in ein Wrapper-Element eingepackt für SAX-Parsing
        String content = new String(Files.readAllBytes(new java.io.File(in).toPath()));
        String wrapped = "<wrapper>" + content + "</wrapper>";

        try (FileOutputStream fos = new FileOutputStream(out)) {
            EXIResult r = new EXIResult(f);
            r.setOutputStream(fos);

            SAXParserFactory spf = SAXParserFactory.newInstance();
            spf.setNamespaceAware(true);
            XMLReader reader = spf.newSAXParser().getXMLReader();

            // Wir nutzen einen Filter-Handler der das wrapper-Element ignoriert
            FragmentContentHandler fragmentHandler = new FragmentContentHandler(r.getHandler());
            reader.setContentHandler(fragmentHandler);
            reader.parse(new InputSource(new StringReader(wrapped)));
        }

        long size = new java.io.File(out).length();
        String modeName = mode == CodingMode.BIT_PACKED ? "bit-packed" : "byte-aligned";
        System.out.println("  " + out + " (" + size + " bytes, " + modeName + ", fragment)");
    }
}

/**
 * ContentHandler der das Wrapper-Element herausfiltert.
 */
class FragmentContentHandler extends DefaultHandler {
    private final org.xml.sax.ContentHandler delegate;
    private int depth = 0;

    FragmentContentHandler(org.xml.sax.ContentHandler delegate) {
        this.delegate = delegate;
    }

    @Override
    public void startDocument() throws org.xml.sax.SAXException {
        delegate.startDocument();
    }

    @Override
    public void endDocument() throws org.xml.sax.SAXException {
        delegate.endDocument();
    }

    @Override
    public void startElement(String uri, String localName, String qName, org.xml.sax.Attributes atts)
            throws org.xml.sax.SAXException {
        depth++;
        if (depth > 1) { // Wrapper überspringen
            delegate.startElement(uri, localName, qName, atts);
        }
    }

    @Override
    public void endElement(String uri, String localName, String qName) throws org.xml.sax.SAXException {
        if (depth > 1) { // Wrapper überspringen
            delegate.endElement(uri, localName, qName);
        }
        depth--;
    }

    @Override
    public void characters(char[] ch, int start, int length) throws org.xml.sax.SAXException {
        if (depth > 1) { // Nur innerhalb der echten Elemente
            delegate.characters(ch, start, length);
        }
    }

    @Override
    public void startPrefixMapping(String prefix, String uri) throws org.xml.sax.SAXException {
        delegate.startPrefixMapping(prefix, uri);
    }

    @Override
    public void endPrefixMapping(String prefix) throws org.xml.sax.SAXException {
        delegate.endPrefixMapping(prefix);
    }
}
