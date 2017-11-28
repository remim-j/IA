package rte;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class DataIA {
	
	private static String inputFile = "YearPredictionMSD.txt";
	private static String trainDataset = "datatrain.csv";
	private static String testDataset = "datatest.csv";

	public static void main(String[] args) {
		
		try {
			BufferedReader bR = new BufferedReader(new FileReader(new File(inputFile)));
			BufferedWriter bW1 = new BufferedWriter(new FileWriter(new File(trainDataset)));
			String lineHeader = "y, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, arg21, arg22, arg23, arg24, arg25, arg26, arg27, arg28, arg29, arg30, arg31, arg32, arg33, arg34, arg35, arg36, arg37, arg38, arg39, arg40, arg41, arg42, arg43, arg44, arg45, arg46, arg47, arg48, arg49, arg50, arg51, arg52, arg53, arg54, arg55, arg56, arg57, arg58, arg59, arg60, arg61, arg62, arg63, arg64, arg65, arg66, arg67, arg68, arg69, arg70, arg71, arg72, arg73, arg74, arg75, arg76, arg77, arg78, arg79, arg80, arg81, arg82, arg83, arg84, arg85, arg86, arg87, arg88, arg89, arg90";
			lineHeader = lineHeader.replace(" ", "");
			bW1.write(lineHeader + "\n");
			BufferedWriter bW2 = new BufferedWriter(new FileWriter(new File(testDataset)));
			bW2.write(lineHeader + "\n");
			String line = "";
			line = bR.readLine();
			int count = 0;
			
			while(line != null) {
				
				if (count < 463715) {
					
					bW1.write(line + "\n");
				}
				
				else {
					
					bW2.write(line + "\n");
				}
				
				count++;
				line = bR.readLine();
			}
			
			bW1.close();
			bW2.close();
			bR.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String getTestDataset() {
		return testDataset;
	}

	public void setTestDataset(String testDataset) {
		DataIA.testDataset = testDataset;
	}

	public String getTrainDataset() {
		return trainDataset;
	}

	public void setTrainDataset(String trainDataset) {
		DataIA.trainDataset = trainDataset;
	}

	public String getInputFile() {
		return inputFile;
	}

	public void setInputFile(String inputFile) {
		DataIA.inputFile = inputFile;
	}
}
