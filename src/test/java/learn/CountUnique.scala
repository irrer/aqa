package learn

object CountUnique {
  val a = Seq(
    "Kilo Voltage Detector",
    "Beam Generation Module",
    "Mega Voltage Detector",
    "XRay Imager",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Supervisor",
    "Mega Voltage Detector",
    "Mega Voltage Detector",
    "Supervisor",
    "Mega Voltage Detector",
    "Supervisor",
    "Collimator",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Stand",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Supervisor",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Steering Adjustment",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Stand",
    "Couch",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Output Adjustment",
    "Stand",
    "Stand",
    "Stand",
    "Stand",
    "Collimator",
    "Collimator",
    "Supervisor",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Output Adjustment",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Steering Adjustment",
    "Supervisor",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Collimator",
    "Collimator",
    "Output Adjustment",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Kilo Voltage Source",
    "Beam Generation Module",
    "Beam Generation Module",
    "Couch",
    "Couch",
    "Beam Generation Module",
    "Beam Generation Module",
    "Collimator",
    "Collimator",
    "Beam Generation Module",
    "Collimator",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Mechanical Jaws",
    "Beam Generation Module",
    "Beam Generation Module",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Output Adjustment",
    "Stand",
    "Kilo Voltage Source",
    "Supervisor",
    "Collimator",
    "Output Adjustment",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Generation Module",
    "Mechanical Jaws",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Beam Generation Module",
    "Mechanical Jaws",
    "Collimator",
    "Collimator",
    "Collimator",
    "Collimator",
    "Beam Generation Module",
    "Beam Generation Module",
    "Beam Steering Adjustment",
    "Beam Steering Adjustment",
    "Beam Generation Module",
    "Output Adjustment"
  )

  //noinspection SpellCheckingInspection
  val b = Seq(
    "Elbow Calibration Data",
    "Energy Independent Parameters / Carrousel / Calibration Data / Transverse",
    "Elbow Calibration Data",
    "XI Configuration",
    "Energy Independent Parameters / Carrousel / Calibration Data / Transverse",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-6xFFF / Dosimetry Calibration",
    "Energies / Energy-10xFFF / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-20e / Dosimetry Calibration",
    "MVD Geometry Data",
    "Arm Geometry Data",
    "Elbow Calibration Data",
    "MVD Geometry Data",
    "Arm Geometry Data",
    "MVD Geometry Data",
    "Calibration Data",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energies / Energy-6xFFF / Dose Diagnostics",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energy Independent Parameters / Carrousel / Calibration Data / Transverse",
    "Energies / Energy-10xFFF / Carrousel / IonChamber",
    "Energy Independent Parameters / Carrousel / Calibration Data / Transverse",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Energies / Energy-12e / Carrousel / Target",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Sound Configuration",
    "Energies / Energy-10xFFF / Carrousel / IonChamber",
    "Energies / Energy-10xFFF / Carrousel / IonChamber",
    "Energy Independent Parameters / Carrousel / Field Group 1",
    "Energy Independent Parameters / Carrousel / Field Group 2",
    "Energy Independent Parameters / Carrousel / Calibration Data / Transverse",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Energies / Energy-10xFFF / Carrousel / IonChamber",
    "MVD Geometry Data",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Delay",
    "Energies / Energy-6x / Carrousel / Energy Switch",
    "Energies / Energy-6x / Delay",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Dose Diagnostics",
    "Recurring BGM faults, re-steer 6x",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energies / Energy-6xFFF / Dose Diagnostics",
    "Energies / Energy-10xFFF / Dose Diagnostics",
    "Sound Configuration",
    "CouchUpper / Couch Longitudinal System CalibData",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-6x / Dosimetry Calibration",
    "TG-51 output adjustment, 6x",
    "Gantry Rotational System CalibData",
    "Cooling",
    "Cooling",
    "Cooling",
    "X1Jaw-Calibration Data Internal",
    "X1Jaw-Calibration Data Internal",
    "MVD Geometry Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energy Independent Parameters / Ion Charge Capacity Test",
    "Energy Independent Parameters / Ion Charge Offsets",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-10xFFF / Symmetry Balance Gain Calibration",
    "Energies / Energy-6xFFF / Symmetry Balance Gain Calibration",
    "Energies / Energy-6e / Beam Steering",
    "Energies / Energy-9e / Symmetry Balance Gain Calibration",
    "Energies / Energy-12e / Symmetry Balance Gain Calibration",
    "Energies / Energy-16e / Beam Steering",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Dose Diagnostics",
    "Energies / Energy-6xFFF / Dosimetry Calibration",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-6xFFF / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "TG-51 output adjustment, all energies",
    "Energies / Energy-16e / Beam Steering",
    "Energies / Energy-16e / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6xFFF / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-15x / Dose Diagnostics",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-10xFFF / Dosimetry Calibration",
    "Varian service adjusted 6x POS steering based on MPC beamcenter results",
    "MVD Geometry Data",
    "Energies / Energy-20e / Beam Steering",
    "Energies / Energy-20e / Dose Diagnostics",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Dose Diagnostics",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energy Independent Parameters / Carrousel / Calibration Data / Target",
    "Energies / Energy-10xFFF / Carrousel / Target",
    "Energies / Energy-6x / Dose Diagnostics",
    "Calibration Data",
    "Calibration Data",
    "TG-51 output adjustment, all energies",
    "Energies / Energy-6e / Dosimetry Calibration",
    "Energies / Energy-9e / Dosimetry Calibration",
    "Energies / Energy-12e / Dosimetry Calibration",
    "Energies / Energy-16e / Dosimetry Calibration",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-15x / Dosimetry Calibration",
    "Energies / Energy-6xFFF / Dosimetry Calibration",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "KVBladeX1 Calibration Data",
    "Energies / Energy-12e / Beam Steering",
    "Energies / Energy-16e / Beam Steering",
    "CouchUpper / Couch Pitch System CalibData",
    "CouchUpper / Couch Roll System CalibData",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-12e / Dose Diagnostics",
    "Calibration Data",
    "Calibration Data",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Y1Jaw-Calibration Data Internal",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-9e / Beam Steering",
    "Energies / Energy-12e / Beam Steering",
    "Energies / Energy-6xFFF / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-6e / Beam Steering",
    "Energies / Energy-6e / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6xFFF / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-6e / Beam Steering",
    "Energies / Energy-9e / Beam Steering",
    "Energies / Energy-12e / Beam Steering",
    "Energies / Energy-16e / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "Varian service replaced mylar, realigned cross-hairs to light field",
    "Energy Independent Parameters / Carrousel / Field Group 1",
    "Energy Independent Parameters / Carrousel / Field Group 2",
    "X1Jaw-Calibration Data System",
    "Y1Jaw-Calibration Data System",
    "Y1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "Calibration Data",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-6xFFF / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Energies / Energy-15x / Beam Steering",
    "Energies / Energy-6e / Beam Steering",
    "Energies / Energy-15x / Dosimetry Calibration",
    "TG-51 output adjustment, 15x",
    "Gantry Rotational System CalibData",
    "KVBladeX1 Calibration Data",
    "MVD Geometry Data",
    "Calibration Data",
    "TG-51 output adjustment, 6x and 6FFF",
    "Energies / Energy-6x / Dosimetry Calibration",
    "Energies / Energy-6xFFF / Dosimetry Calibration",
    "Energies / Energy-10xFFF / Dosimetry Calibration",
    "Energies / Energy-6x / Beam Steering",
    "Energies / Energy-10xFFF / Beam Steering",
    "Collimator: x1, x2, y1, y2 jaw calibration",
    "Y1Jaw-Calibration Data System",
    "Y2Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "X1Jaw-Calibration Data System",
    "Energy Independent Parameters / Carrousel / Calibration Data / Energyswitch",
    "Varian service adjusted Y-jaw calibration based on MPC beamcenter results",
    "Y2Jaw-Calibration Data System",
    "Y2Jaw-Calibration Data System",
    "Y2Jaw-Calibration Data System",
    "Y2Jaw-Calibration Data System",
    "Energies / Energy-20e / Beam Steering",
    "Energies / Energy-6x / Beam Steering",
    "6x beam steering",
    "6x beam steering",
    "Energy Independent Parameters / Ion Charge Nominal Test",
    "TG-51 output adjustment, all energies"
  )
  def main(args: Array[String]): Unit = {

    println("a.size: " + a.size)
    println("b.size: " + b.size)

    val both = a.zip(b).map(ab => ab._1 + " : " + ab._2).distinct
    both.take(10).foreach(println)
    println("both.size: " + both.size)

  }
}
