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
