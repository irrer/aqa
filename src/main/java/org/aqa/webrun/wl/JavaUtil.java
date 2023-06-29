package org.aqa.webrun.wl;

public class JavaUtil {
    @SuppressWarnings("ForLoopReplaceableByForEach")
    public static float[][] pixelDataToArray(int height, int width, short[] shorts, boolean flip) {
        float[][] array = new float[height][width];
        int index = 0;
        for (int y = 0; y < height; y++) {
            float[] row = array[y];
            for (int x = 0; x < width; x++) {
                row[x] = shorts[index] & 0xffff;
                index++;
            }
        }
        if (flip) {
            float min = array[0][0];
            float max = array[0][0];
            for (int y = 0; y < array.length; y++) {
                for (int x = 0; x < array[0].length; x++) {
                    float v = array[y][x];
                    min = Math.min(min, v);
                    max = Math.max(max, v);
                }
            }
            for (int y = 0; y < array.length; y++) {
                for (int x = 0; x < array[0].length; x++) {
                    array[y][x] = max - array[y][x];
                }
            }
        }
        return array;
    }
}
