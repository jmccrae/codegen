package eu.liderproject.codegen.example;

import org.unileipzig.persistence.nlp2rdf.ontologies.nifcore.String_;

public class Tokenizer {
    public String_ tokenize(String_ nifString) {
        final String string = nifString.getAnchorOf().iterator().next();
        final String[] tokenized = string.replaceAll("(\\.\\.\\.+|[\\p{Po}\\p{Ps}\\p{Pe}\\p{Pi}\\p{Pf}\u2013\u2014\u2015&&[^'\\.]]|(?<!(\\.|\\.\\p{L}))\\.(?=[\\p{Z}\\p{Pf}\\p{Pe}]|\\Z)|(?<!\\p{L})'(?!\\p{L}))"," $1 ")
            .replaceAll("\\p{C}|^\\p{Z}+|\\p{Z}+$","")
            .split("\\p{Z}+");

        final String_ nifTokenized = new String_("result");
        nifTokenized.addAnchorOf(string);

        for(int i = 0; i < tokenized.length; i++) {
            final String_ token = new String_("token" + i);
            token.addAnchorOf(tokenized[i]);
            nifTokenized.addSubString(token);
        }        

        return nifTokenized;
    }
}
