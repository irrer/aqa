package learn;

import java.io.File;
import java.io.IOException;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.data.general.DefaultPieDataset;

public class ChartAsPNG {

    public static void main(String[] args) throws IOException {

        //Prepare the data set
        DefaultPieDataset pieDataset = new DefaultPieDataset();
        pieDataset.setValue("Coca-Cola", 26);
        pieDataset.setValue("Pepsi", 20);
        pieDataset.setValue("Gold Spot", 12);
        pieDataset.setValue("Slice", 14);
        pieDataset.setValue("Appy Fizz", 18);
        pieDataset.setValue("Limca", 10);

        //Create the chart
        JFreeChart chart = ChartFactory.createPieChart3D(
                "Soft Dink 3D Pie Chart", pieDataset, true, true, true);

        //Save chart as PNG
        ChartUtilities.saveChartAsPNG(new File("D:\\tmp\\pop_pie.png"), chart, 400, 300);
    }

}
