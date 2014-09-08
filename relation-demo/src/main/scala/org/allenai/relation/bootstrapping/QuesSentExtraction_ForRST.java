package org.allenai.relation.bootstrapping;
//package questions;
//
//import java.io.BufferedWriter;
//import java.io.File;
//import java.io.FileInputStream;
//import java.io.FileWriter;
//import java.io.IOException;
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.Iterator;
//import java.util.Map;
//import java.util.TreeMap;
//
//import org.apache.poi.ss.usermodel.Row;
//import org.apache.poi.xssf.usermodel.XSSFRow;
//import org.apache.poi.xssf.usermodel.XSSFSheet;
//import org.apache.poi.xssf.usermodel.XSSFWorkbook;
//
//public class QuesSentExtraction_ForRST {
//	public static int Q_Index = 0;
//	public static int R_Q_Index = 1;
//	public static int R_Q_D_Index = 2;
//	public static int RC_Q_Index = 3;
//	public static int QR_Index = 4;
//	public static int R_QR_Index = 5;
//	public static int IS_Index = 6;
//	public static int IS_ID_Index = 7;
//	public static int AR_S_Index = 8;
//	public static int R_S_Index = 9;
//	public static int R_S_D_Index = 10;
//	public static int RC_S_Index = 11;
//	public static int IR_Index = 12;
//	public static int R_IR_Index = 13;
//	
//	public static HashMap<String, MPair<String, String>> map = 
//			new HashMap<String, MPair<String, String>>();
//	public static void main(String[] args) {
//		String iFileName = "data/Question Analysis Sheet.xlsx";
//		String oFileDirectory = "data/QuestionSentenceExtraction";
//		
//		String specRel = null;
//		String oFileName = null;
//		
//		
//		readExcel(iFileName);
//		
//		printMap();
//		writeMap(oFileDirectory, true);
//	}
//	
//	public static void readExcel(String iFileName) {
//		try {
//			FileInputStream file = new FileInputStream(new File(iFileName));
//			//create workbook instance holding reference to .xlsx file
//			XSSFWorkbook workbook = new XSSFWorkbook(file);
//			
//			//get first/desired sheet from the workbook
//			XSSFSheet sheet = workbook.getSheetAt(0);
//			
//			//the headline of output sheet
//			Map<String, Object[]> data = new TreeMap<String, Object[]> ();
//			data.put("1", new Object[] {"Q/S", "RA"});
//			int rowNum = 2;
//			
//			//iterate through each rows one by one
//			Iterator<Row> rowIterator = sheet.rowIterator();
//			rowIterator.next();
//			while(rowIterator.hasNext()) {
//				XSSFRow row = (XSSFRow) rowIterator.next();
//				
//				if(row.getCell(Q_Index) != null) {
//					String Q = row.getCell(Q_Index).toString();
//					if(!Q.equals("None") && !Q.equals("NA") && !Q.equals("several sentences")) {
//						ArrayList<String> TFQs = MCToTF.GURL(Q);
//						int m = 1;
//						for(String tfq : TFQs) {
//							String R_Q = row.getCell(R_Q_Index).toString();
//							String file_index = rowNum+"_"+Q_Index+"_"+m;
//							MPair<String, String> q_r = new MPair<String, String>(tfq, R_Q);
//							map.put(file_index, q_r);
//							m++;
//						}
//					}
//					
//					String S = row.getCell(IS_Index).toString();
//					if(!S.equals("None") && !S.equals("NA") && !S.equals("several sentences")) {
//						String R_S = row.getCell(R_S_Index).toString();
//						String file_index = rowNum+"_"+IS_Index;
//						MPair<String, String> s_r = new MPair<String, String>(S, R_S);
//						map.put(file_index, s_r);
//					}
//				}
//				
//				rowNum++;
//				file.close();
//			}
//		} catch (Exception e) {
//			e.printStackTrace();
//		}
//	}
//	
//	public static ArrayList<String> extractRNames(String str) {
//		ArrayList<String> rnames_list =new ArrayList<String>();
//		String[] arr = str.split("\\(");
//		for(String a : arr) {
//			int len = a.length();
//			int end = len-1;
//			int beg = 0;
//			for(int i=len-1; i>=0; i--) {
//				if(!(a.charAt(i)>='A' && a.charAt(i)<='Z')) {
//					beg = i+1;
//					break;
//				}
//			}
//			String rname = a.substring(beg, end+1);
//			if(!rnames_list.contains(rname)) {
//				rnames_list.add(rname);
//			}
//		}
//		return rnames_list;
//	}
//	
//	public static void printMap() {
//		for(String key : map.keySet()) {
//			MPair<String, String> pair = map.get(key);
//			System.out.println(key+"\t"+pair.getFirst()+"\t"+pair.getSecond());
//		}
//	}
//	
//	public static void writeMap(String oFileDir, boolean isRewrite) {
//		String oFileDir_s = oFileDir + "/quessent";
//		File writeFile_s = new File(oFileDir_s);
//        if(!writeFile_s.exists())
//            writeFile_s.mkdirs();
//        
//        String oFileDir_r = oFileDir + "/relation";
//        File writeFile_r = new File(oFileDir_r);
//        if(!writeFile_r.exists())
//        	writeFile_r.mkdirs();
//        
//		try{
//			for(String key : map.keySet()) {
//				//write the question or sentence
//				String oFileName_s = oFileDir_s + "/" + key + ".txt";
//				File file_s = new File(oFileName_s);
//				BufferedWriter bw_s = null;
//				if(isRewrite==true)
//					bw_s = new BufferedWriter(new FileWriter(file_s));
//				else
//					bw_s = new BufferedWriter(new FileWriter(file_s, true));
//				
//				bw_s.write(map.get(key).getFirst());
//				bw_s.write("<s>");
//				bw_s.newLine();
//				
//				//write the relation
//				String oFileName_r = oFileDir_r + "/" + key + ".txt";
//				File file_r = new File(oFileName_r);
//				BufferedWriter bw_r = null;
//				if(isRewrite==true)
//					bw_r = new BufferedWriter(new FileWriter(file_r));
//				else
//					bw_r = new BufferedWriter(new FileWriter(file_r, true));
//				
//				bw_r.write(map.get(key).getSecond());
//				bw_r.newLine();
//				
//				bw_s.flush();
//				bw_s.close();
//				bw_r.flush();
//				bw_r.close();
//			}
//		}catch(IOException ioe){
//			System.out.println("Fail to create the file!");
//		}
//	}
//}