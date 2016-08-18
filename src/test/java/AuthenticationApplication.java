
import org.restlet.Application;
import org.restlet.Component;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.Restlet;
import org.restlet.data.ChallengeScheme;
import org.restlet.data.Protocol;
import org.restlet.data.ChallengeMessage;
import org.restlet.data.ChallengeRequest;
import org.restlet.data.ChallengeResponse;
import org.restlet.resource.Directory;
import org.restlet.routing.Filter;
import org.restlet.security.ChallengeAuthenticator;
import org.restlet.security.MapVerifier;

public class AuthenticationApplication extends Application {

    public static void main(String[] args) throws Exception {
        Component c = new Component();
        // server listening on port 8182
        c.getServers().add(Protocol.HTTP, 8181);
        // client connector required by the Directory.
        c.getClients().add(Protocol.FILE);
        c.getDefaultHost().attach(new AuthenticationApplication());

        c.start();
    }

    class Fltr extends Filter {

        private boolean isOk(Request request) {
            
            ChallengeMessage cm = null;
            ChallengeRequest crq = null;
            ChallengeResponse crs = null;
            
            if (cm == null) ;
            if (crq == null) ;
            if (crs == null) ;
            
            ChallengeResponse cr = request.getChallengeResponse();
            if (cr == null) {
                System.out.println("no ChallengeResponse");
                return false;
            } else {
                String id = cr.getIdentifier();
                String secret = new String(cr.getSecret());
                boolean ok = id.equals("jim") && secret.equals("foo");
                System.out.println("id: " + id + "    secret: " + secret + "    ok: " + ok);
                return ok;
            }
        }

        protected int beforeHandle(Request request, Response response) {
            System.out.println("beforeHandle");
            if (isOk(request))
                return CONTINUE;
            else
                return SKIP;
        }

        protected void afterHandle(Request request, Response response) {
            System.out.println("afterHandle");
            if (!isOk(request)) {
                response.redirectSeeOther("http://localhost:8181/HorseyHundred/");
                //response.redirectSeeOther("http://localhost:8181/HorseyHundred");
            }
        }

    }

    Directory horsey = new Directory(getContext(), "file:///D:\\tmp\\HorseyHundred");

    @Override
    public Restlet createInboundRoot() {
        // Create a simple password verifier
        MapVerifier verifier = new MapVerifier();
        verifier.getLocalSecrets().put("jim", "foo1".toCharArray());

        Fltr fltr = new Fltr();

        // Create a guard
        ChallengeAuthenticator guard = new ChallengeAuthenticator(getContext(), true, ChallengeScheme.HTTP_BASIC, "Tutorial");
        guard.setVerifier(verifier);

        horsey.setListingAllowed(true);

        // Create a Directory able to return a deep hierarchy of files
        Directory directory = new Directory(getContext(), "file:///D:\\tmp");
        directory.setListingAllowed(true);
        guard.setNext(directory);

        fltr.setNext(directory);
        guard.setNext(fltr);
        return guard;
    }

}
