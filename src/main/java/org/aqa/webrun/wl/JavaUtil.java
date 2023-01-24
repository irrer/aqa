package org.aqa.webrun.wl;

import edu.umro.util.OpSys;

public class JavaUtil {

    public static float[][] pixelDataToArray(int height, int width, short[] shorts) {
        float[][] array = new float[height][width];

        int index = 0;
        for (int y = 0; y < height; y++) {
            float[] row = array[y];
            for (int x = 0; x < width; x++) {
                row[x] = shorts[index] & 0xffff;
                index++;
            }
        }

        return array;
    }

    /**
     * Determine if this the system on which software development is done.
     */
    public static boolean isDevelopmentSystem() {
        return OpSys.getHostIPAddress().equalsIgnoreCase("141.214.125.68");
    }


}
