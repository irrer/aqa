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

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

public class SqlServerConnect {

	public static void main(String[] args) {

		// String connectionUrl =
		// "jdbc:sqlserver://yourserver.database.windows.net:1433;" +
		// "database=AdventureWorks;" + "user=yourusername@yourserver;" +
		// "password=yourpassword;"
		// + "encrypt=true;" + "trustServerCertificate=false;" + "loginTimeout=30;";

		File cur = new File(".");
		System.out.println(cur.getAbsolutePath());

		if (true) {
			String dll = "D:\\pf\\eclipse\\workspaceOxygen\\aqa\\sqljdbc_auth.dll";
			// String dll =
			// "D:\\pf\\eclipse\\workspaceOxygen\\aqa\\src\\main\\dll\\x64\\sqljdbc_auth.dll";
			File dllFile = new File(dll);
			System.out.println("can read file " + dllFile.getAbsolutePath() + "    canRead: " + dllFile.canRead() + "    canExecute: " + dllFile.canExecute());
			System.load(dll);
			System.out.println("Loaded " + dll);
		}

		String boo[] = { "true", "false" };

		for (int en = 0; en < 2; en++) {
			for (int t = 0; t < 2; t++) {
				String encrypt = boo[en];
				String trustServerCertificate = boo[t];
				String connectionUrl = "jdbc:sqlserver://NTSRODBSDV1.UMHS.MED.UMICH.EDU:1433;" +

						"database=AQAmsDV;" +

						"user=UMHS\\irrer;" +
						// "user=UMHS\\umhs-irrer;" +

						"password=45eetslp;" +
						// "password=d2eypdQb3w;" +

						"encrypt=" + encrypt + ";" +

						"trustServerCertificate=" + trustServerCertificate + ";" +

						"loginTimeout=30;";

				System.out.println("    encrypt=" + encrypt + ";" + "    trustServerCertificate=" + trustServerCertificate);
				try (Connection connection = DriverManager.getConnection(connectionUrl);) {
					// Code here.
				}
				// Handle any errors that may have occurred.
				catch (SQLException ex) {
					System.out.println(ex);
					// e.printStackTrace();
				}
			}
		}
	}

}
