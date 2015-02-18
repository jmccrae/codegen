package org.unileipzig.persistence.nlp2rdf.ontologies.nifcore;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Statement;
import com.hp.hpl.jena.rdf.model.StmtIterator;
import static java.util.Collections.*;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import javax.persistence.Id;

/**
 * Automatically generated POJOs for ontology at http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#String.
 * DO NOT EDIT! 
 */
public class String_ {
    public final static String __uri__ = "http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#String";
    private final String id;

    public String_(String id) {
        this.id = id;
    }

    private final Set<String_> subString = new HashSet<String_>();

    public Set<String_> getSubString() {
        return unmodifiableSet(this.subString);
    }

    public void setSubString(Set<String_> subString) {
        this.subString.clear();
        this.subString.addAll(subString);
    }

    public void addSubString(String_ subString) {
        this.subString.add(subString);
    }

    public boolean removeSubString(String_ subString) {
        return this.subString.remove(subString);
    }

    private final Set<String> anchorOf = new HashSet<String>();

    public Set<String> getAnchorOf() {
        return unmodifiableSet(this.anchorOf);
    }

    public void setAnchorOf(Set<String> anchorOf) {
        this.anchorOf.clear();
        this.anchorOf.addAll(anchorOf);
    }

    public void addAnchorOf(String anchorOf) {
        this.anchorOf.add(anchorOf);
    }

    public boolean removeAnchorOf(String anchorOf) {
        return this.anchorOf.remove(anchorOf);
    }


    public static String_ fromResource(Resource resource) {
        final String id;
        if(resource.isURIResource()) {
            id = resource.getModel().shortForm(resource.getURI());
        } else {
            id = "_:" + resource.getId().getLabelString();
        }
        final String_ thisObj = new String_(id);
        {
            final StmtIterator iterator = resource.listProperties(
                resource.getModel().getProperty("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#subString"));

            while(iterator.hasNext()) {
                final Statement statement = iterator.next();
                if(statement.getObject().isResource()) {
                    thisObj.addSubString(String_.fromResource(
                        statement.getObject().asResource()));
                } else {
                    throw new RuntimeException("Expected a resource for http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#subString");
                }
            }
        }
        {
            final StmtIterator iterator = resource.listProperties(
                resource.getModel().getProperty("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#anchorOf"));

            while(iterator.hasNext()) {
                final Statement statement = iterator.next();
                if(statement.getObject().isLiteral()) {
                    thisObj.addAnchorOf(statement.getObject().asLiteral().getString());
                } else {
                    throw new RuntimeException("Expected a literal for http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#anchorOf");
                }
            }
        }
        return thisObj;
    }

    public Resource toResource(Model model) {
        final Resource resource = model.createResource(model.expandPrefix(id)); 
        for(String_ member : subString) {
            resource.addProperty(model.createProperty("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#subString"), member.toResource(model));
        }
        for(String member : anchorOf) {
            resource.addProperty(model.createProperty("http://persistence.uni-leipzig.org/nlp2rdf/ontologies/nif-core#anchorOf"), member);
        }
        return resource;
    }
}
