package org.aqac.web

import org.restlet.Restlet
import org.restlet.Request
import org.restlet.Response
import org.restlet.data.Method
import java.util.Date

class CompDb extends Restlet {

    private val content =

        // <!DOCTYPE html>
        <html>
            <head>
                <title>Computers database</title>
                <link rel="stylesheet" type="text/css" media="screen" href="/assets/stylesheets/bootstrap.min.css"/>
                <link rel="stylesheet" media="screen" href="/assets/stylesheets/main.css"/>
            </head>
            <body>
                <header class="topbar">
                    <h1 class="fill">
                        <a href="/">
                            Play sample application &mdash; Computer database
                        </a>
                    </h1>
                </header>
                <section id="main">
                    <h1>Add a computer</h1>
                    <form action="/computers" method="POST">
                        <fieldset>
                            <div class="clearfix ">
                                <label for="name">Computer name</label>
                                <div class="input">
                                    <input type="text" id="name" name="name" value=""/>
                                    <span class="help-inline"></span>
                                </div>
                            </div>
                            <div class="clearfix ">
                                <label for="introduced">Introduced date</label>
                                <div class="input">
                                    <input type="date" id="introduced" name="introduced" value=""/>
                                    <span class="help-inline"></span>
                                </div>
                            </div>
                            <div class="clearfix ">
                                <label for="discontinued">Discontinued date</label>
                                <div class="input">
                                    <input type="date" id="discontinued" name="discontinued" value=""/>
                                    <span class="help-inline"></span>
                                </div>
                            </div>
                            <div class="clearfix ">
                                <label for="company">Company</label>
                                <div class="input">
                                    <select id="company" name="company">
                                        <option class="blank" value="">-- Choose a company --</option>
                                        <option value="22">Acorn computer</option>
                                        <option value="29">ACVS</option>
                                        <option value="14">Amiga Corporation</option>
                                        <option value="38">Amstrad</option>
                                        <option value="26">Xerox</option>
                                        <option value="28">Zemmix</option>
                                    </select>
                                    <span class="help-inline"></span>
                                </div>
                            </div>
                        </fieldset>
                        <div class="actions">
                            <input type="submit" value="Create this computer" class="btn primary"/>
                            or
                            <a href="/computers" class="btn">Cancel</a>
                        </div>
                    </form>
                </section>
            </body>
        </html>;

    override def handle(request: Request, response: Response): Unit = {
        if (request.getMethod == Method.GET) {
            WebUtil.respond(content, "Create Institution", response)
        }
        else WebUtil.notFound(response)
    }
}