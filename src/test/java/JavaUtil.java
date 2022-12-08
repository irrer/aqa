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


import edu.umro.util.OpSys;

public class JavaUtil {

    public static int[][] pixelDataToArray(int height, int width, short[] shorts) {
        int[][] array = new int[height][width];

        int index = 0;
        for (int y = 0; y < height; y++) {
            int[] row = array[y];
            for (int x = 0; x < width; x++) {
                row[x] = shorts[index] & 0xffff;
                index++;
            }
        }

        return array;
    }

    /** Determine if this the system on which software development is done. */
    public static boolean isDevelopmentSystem() {
        return OpSys.getHostIPAddress().equalsIgnoreCase("141.214.125.68");
    }

}
