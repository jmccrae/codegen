package eu.liderproject.codegen.server;

import java.io.FileReader;
import java.util.List;
import java.util.Arrays;
import org.eclipse.jetty.server.Server;

public class Main {
    public static void main(String[] _args) throws Exception {
        final List<String> args = Arrays.asList(_args);
        int port = 8080;
        for(int i = 0; i < args.size() - 1; i++) {
            if(args.get(i).equals("-p")) {
                port = Integer.parseInt(args.get(i + 1));
                args.remove(i);
                args.remove(i);
            }
        }

        if(args.size() != 1) {
            System.err.println("Usage: codegen-server [-p port] config.json");
            System.exit(-1);
        }

        final Server server = new Server(port);
        server.setHandler(new CodegenServerHandler(new FileReader(args.get(0))));
        
        server.start();
        server.join();
    }
}
