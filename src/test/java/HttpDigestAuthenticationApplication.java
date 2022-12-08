/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


// package org.restlet.example.authentication;

import org.restlet.Application;
import org.restlet.Component;
import org.restlet.Restlet;
import org.restlet.data.Protocol;
import org.restlet.ext.crypto.DigestAuthenticator;
import org.restlet.resource.Directory;
import org.restlet.security.MapVerifier;

public class HttpDigestAuthenticationApplication extends Application {

    public static void main(String[] args) throws Exception {
        Component c = new Component();
        // server listening on pport 8182
        c.getServers().add(Protocol.HTTP, 8182);
        // client connector required by the Directory.
        c.getClients().add(Protocol.FILE);
        c.getDefaultHost().attach(new HttpDigestAuthenticationApplication());

        c.start();
    }

    @Override
    public Restlet createInboundRoot() {
        // Create a simple password verifier
        MapVerifier verifier = new MapVerifier();
        verifier.getLocalSecrets().put("scott", "tiger".toCharArray());

        DigestAuthenticator guard = new DigestAuthenticator(getContext(), "TestRealm", "mySecretServerKey");
        MapVerifier mapVerifier = new MapVerifier();
        mapVerifier.getLocalSecrets().put("scott", "tiger".toCharArray());
        guard.setWrappedVerifier(mapVerifier);

        // Create a Directory able to return a deep hierarchy of files
        Directory directory = new Directory(getContext(), "file:///D:\\tmp");
        directory.setListingAllowed(true);
        guard.setNext(directory);

        return guard;
    }

}
