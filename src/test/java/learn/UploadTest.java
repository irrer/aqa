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

package learn;

import org.apache.commons.fileupload.FileItemIterator;
import org.apache.commons.fileupload.FileItemStream;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.restlet.Application;
import org.restlet.Restlet;
import org.restlet.Component;
import org.restlet.routing.Router;
import org.restlet.data.Protocol;
import org.restlet.Request;
import org.restlet.Response;
import org.restlet.data.Status;
import org.restlet.ext.fileupload.RestletFileUpload;
import org.restlet.representation.Representation;
import org.restlet.representation.StringRepresentation;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Date;

import org.restlet.data.Form;
import org.restlet.data.MediaType;

public class UploadTest extends Application {
    private static final int HTTP_PORT = 9050;
    private static final String TITLE = "Minimal File Upload " + (new Date());

    private String html() {
        String text = "<!DOCTYPE html>\n" +
                "<html lang='en'>\n" +
                "    <head><title>" + TITLE + "</title></head>\n" +
                "    <body>\n" +
                "        <h2>" + TITLE + "</h2><p></p>\n" +
                "        <form action='/' method='post' enctype='multipart/form-data'>\n" +
                "            Upload a file " + (new Date()) + "<p></p>\n" +
                "            <input type='file' name='fileToUpload'/><p></p>\n" +
                "            <input type='submit' VALUE='Submit'/>\n" +
                "        </form>\n" +
                "    </body>\n" +
                "</html>\n" +
                "\n";
        return text;
    }

    @Override
    public Restlet createInboundRoot() {
        return new Router(getContext().createChildContext());
    }

    private UploadTest() throws Exception {
        System.out.println("Using port " + HTTP_PORT);
        Component component = new Component();
        component.getServers().add(Protocol.HTTP, HTTP_PORT);
        component.getDefaultHost().attach(this);
        component.start();
    }

    private void processFile(Request request) throws Exception {
        Representation entity = request.getEntity();
        if (entity != null) {
            MediaType mt = entity.getMediaType();
            System.out.println("mt: " + mt);
            if (MediaType.MULTIPART_FORM_DATA.equals(entity.getMediaType(), true)) {
                // 1/ Create a factory for disk-based file items
                DiskFileItemFactory factory = new DiskFileItemFactory();
                factory.setSizeThreshold(1000240);

                // 2/ Create a new file upload handler based on the Restlet
                // FileUpload extension that will parse Restlet requests and
                // generates FileItems.
                RestletFileUpload upload = new RestletFileUpload(factory);

                // 3/ Request is parsed by the handler which generates a
                // list of FileItems
                FileItemIterator fileIterator = upload.getItemIterator(entity);

                // Process only the uploaded item called "fileToUpload"
                // and return back
                boolean found = false;
                while (fileIterator.hasNext() && !found) {
                    FileItemStream fi = fileIterator.next();
                    if (fi.getFieldName().equals("fileToUpload")) {
                        found = true;
                        // consume the stream immediately, otherwise the stream
                        // will be closed.
                        StringBuilder sb = new StringBuilder("media type: ");
                        sb.append(fi.getContentType()).append("\n");
                        System.out.println("fi.getContentType(): " + fi.getContentType());
                        sb.append("file name : ");
                        sb.append(fi.getName()).append("\n");
                        if (System.out != null) {
                            InputStream inStr = fi.openStream();
                            File outFile = new File("D:\\tmp\\aqa\\outFile");
                            outFile.createNewFile();
                            outFile.delete();
                            byte[] buf = new byte[62 * 1024];
                            FileOutputStream fos = new FileOutputStream(outFile);
                            int len = 0;
                            while ((len = inStr.read(buf)) != -1) {
                                fos.write(buf, 0, len);
                            }
                            fos.flush();
                            fos.close();
                            fos = null;

                        } else {
                            BufferedReader br = new BufferedReader(new InputStreamReader(fi.openStream()));
                            String line = null;
                            while ((line = br.readLine()) != null) {
                                sb.append(line);
                            }
                            sb.append("\n");
                            StringRepresentation result = new StringRepresentation(sb.toString(), MediaType.TEXT_PLAIN);
                        }
                        //System.out.println("result: " + result);
                    }
                }
            }
        }
    }

    private void processFile1(Request request) {
        try {
            System.out.println("request: " + request);
            Representation entity = request.getEntity();
            if (entity != null) {

                //Form form = new Form(entity);
                //System.out.println("form: " + form);

                System.out.println("entity: " + entity);
                System.out.println("entity.getSize(): " + entity.getSize());
                InputStream inputStream = entity.getStream();
                System.out.println("inputStream: " + inputStream.toString());
                if (inputStream != null) {
                    int b = 0;
                    while ((b = inputStream.read()) != -1) {
                        System.out.println("    --> " + (char) b);
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @Override
    public void handle(Request request, Response response) {
        try {
            processFile(request);
            response.setStatus(Status.SUCCESS_OK);
            String content = "UploadTest : " + (new Date());
            content = html();
            response.setEntity(content, MediaType.TEXT_HTML);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        try {
            System.out.println("Starting...");
            new UploadTest();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
