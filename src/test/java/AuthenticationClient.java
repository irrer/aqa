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

//package org.restlet.example.authentication;

import org.restlet.data.ChallengeResponse;
import org.restlet.data.ChallengeScheme;
import org.restlet.data.Status;
import org.restlet.resource.ClientResource;

public class AuthenticationClient {

    public static void main(String[] args) {
        // Prepare the request
        ClientResource resource = new ClientResource("http://localhost:8182/");

        // Add the client authentication to the call
        ChallengeScheme scheme = ChallengeScheme.HTTP_BASIC;
        ChallengeResponse authentication = new ChallengeResponse(scheme,
                "scott", "tiger");
        resource.setChallengeResponse(authentication);

        try {
            // Send the HTTP GET request
            resource.get();
            // Output the response entity on the JVM console
            resource.getResponseEntity().write(System.out);
        } catch (Exception e) {
            if (Status.CLIENT_ERROR_UNAUTHORIZED.equals(resource.getStatus())) {
                // Unauthorized access
                System.out
                        .println("Access unauthorized by the server, check your credentials");
            } else {
                // Unexpected status
                System.out.println("An unexpected status was returned: "
                        + resource.getStatus());
            }
        }
    }
}
