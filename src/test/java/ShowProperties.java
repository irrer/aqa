
import java.util.Properties;

public class ShowProperties {

	/** Determine if this the system on which software development is done. */
	public static void main(String[] args) {
		Properties propList = System.getProperties();
		for (Object o : propList.keySet()) {
			String key = o.toString();
			System.out.println("    " + key + " : " + propList.getProperty(key));
		}
	}

}
