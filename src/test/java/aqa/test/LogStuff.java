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
