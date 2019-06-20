
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
