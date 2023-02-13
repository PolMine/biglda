// javac -cp /opt/mallet/Mallet-202108/class BigTopicModelDiagnostics.java
// then
// mv BigTopicModelDiagnostics.class ~/Lab/github/biglda/inst/java/


import java.io.*;
import java.util.*;
import java.text.*;

import cc.mallet.topics.*;
import cc.mallet.types.*;
import cc.mallet.util.*;

public class BigTopicModelDiagnostics extends TopicModelDiagnostics {

  int numTopics;
	int numTopWords;

	public static final int TWO_PERCENT_INDEX = 1;
	public static final int FIFTY_PERCENT_INDEX = 6;
	public static final double[] DEFAULT_DOC_PROPORTIONS = { 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5 };

	/**  All words in sorted order, with counts */
	ArrayList<TreeSet<IDSorter>> topicSortedWords;
	
	/** The top N words in each topic in an array for easy access */
	String[][] topicTopWords;

	ArrayList<TopicScores> diagnostics; 

	RTopicModel model;
	Alphabet alphabet;

	int[][][] topicCodocumentMatrices;

	int[] numRank1Documents;
	int[] numNonZeroDocuments;
	int[][] numDocumentsAtProportions;

	// This quantity is used in entropy calculation
	double[] sumCountTimesLogCount;

	int[] wordTypeCounts;
	int numTokens = 0;


	public BigTopicModelDiagnostics(BigTopicModel model, int numTopWords) {
	  super(model, numTopWords);
	}
}