package aqa.test;

import java.util.logging.Logger;

import java.io.IOException;

public class LogStuff {

	public static void main(String[] args) throws IOException {
		System.out.println("hello");

		System.out.println("bye");

	}

	public static void doit() {
		System.out.println("doit");
		String cls = LogStuff.class.getName();
		System.out.println("cls: " + cls);
		Logger logger = Logger.getLogger(cls);
		logger.info("info from doit");
		System.out.println("didit");

	}
}
