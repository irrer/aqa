package org.aqa.run

trait PostProcess {
    def postPerform(activeProcess: ActiveProcess): Unit = {}
}