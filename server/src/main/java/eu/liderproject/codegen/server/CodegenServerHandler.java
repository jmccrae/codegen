package eu.liderproject.codegen.server;

import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.ResIterator;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.vocabulary.RDF;
import com.google.gson.Gson;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Collection;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.eclipse.jetty.server.Request;
import org.eclipse.jetty.server.handler.AbstractHandler;
 
public class CodegenServerHandler extends AbstractHandler
{
    private final Configuration configuration;

    public CodegenServerHandler(Reader configFile) {
        this.configuration = readConfig(configFile);
    }

    static class ServiceConfiguration {
        String jar;
        String className;
        Collection<String> methods;
        String path;

        private Class<?> clazz;
        public Class<?> getServiceClass() throws MalformedURLException, ClassNotFoundException {
            if(clazz != null) {
                return clazz;
            }
            if(jar != null) {
                final URL url = new URL("jar:file:" + jar + "!/");
                final URL[] urls = { url };
                final URLClassLoader cl = new URLClassLoader(urls, CodegenServerHandler.class.getClassLoader());
                return clazz = cl.loadClass(className);
            } else {
                return clazz = CodegenServerHandler.class.getClassLoader().loadClass(className);
            }
        }
    }

    static class Configuration {
        Collection<ServiceConfiguration> services;
    }

    private static Configuration readConfig(Reader configFile) {
        final Gson gson = new Gson();
        return gson.fromJson(configFile, Configuration.class);
    }
 
    public void handle(String target,
                       Request baseRequest,
                       HttpServletRequest request,
                       HttpServletResponse response) throws IOException, ServletException {
        final String path = request.getPathInfo();
        for(ServiceConfiguration serviceConfiguration : configuration.services) {
            for(String method : serviceConfiguration.methods) {
                if(path.equals(serviceConfiguration.path + method)) {
                    try {
                        serviceHandle(request, response, serviceConfiguration, method);
                    } catch(Exception x) {
                        x.printStackTrace();
                        throw new ServletException(x);
                    }
                    baseRequest.setHandled(true);
                    return;
                }
            }
        }

        response.sendError(HttpServletResponse.SC_NOT_FOUND);
 
        baseRequest.setHandled(true);
    }

    private String uriForClass(Class<?> clazz) throws Exception {
        return (String)clazz.getField("__uri__").get(null);
    }

    private Object fromResource(Resource resource, Class<?> as) throws Exception {
        return as.getMethod("fromResource", Resource.class).invoke(null, resource);
    }

    private void toResource(Object result, Class<?> as, Model model) throws Exception {
        as.getMethod("toResource", Model.class).invoke(result, model);
    }

    private void serviceHandle(HttpServletRequest request,
                               HttpServletResponse response,
                               ServiceConfiguration config,
                               String methodName) throws Exception {
        // 1. Find the class
        final Class<?> serviceClass = config.getServiceClass();
        System.err.println(String.format("Handling with %s", serviceClass.getName()));
        Method method = null;
        for(Method m : serviceClass.getDeclaredMethods()) {
            if(m.getName().equals(methodName) && m.getParameterTypes().length == 1) {
                method = m;
            }
        }
        if(method == null) {
            throw new NoSuchMethodException();
        }
        
        // 2. Deserialize
        final Model model = ModelFactory.createDefaultModel();
        final Model resultModel = ModelFactory.createDefaultModel();
        model.read(request.getInputStream(), request.getRequestURL().toString(), "TURTLE");
        final ResIterator subjects = model.listSubjectsWithProperty(RDF.type,
                model.createResource(uriForClass(method.getParameterTypes()[0])));

        while(subjects.hasNext()) {
            final Resource resource = subjects.next();
            final Object argument = fromResource(resource, method.getParameterTypes()[0]);
            Object instance = null;
            try {
                instance = serviceClass.newInstance();
            } catch(Exception x) {
                instance = null;
            }
            final Object result = method.invoke(instance, argument);
            toResource(result, method.getReturnType(), resultModel);
        }

        // 3. Serialize
        response.setContentType("text/turtle;utf-8");
        resultModel.write(response.getOutputStream(), "TURTLE");
    } 
}
