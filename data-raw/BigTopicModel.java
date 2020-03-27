// javac -cp /opt/mallet-2.0.8/class/ BigTopicModel.java
// then move it to ./biglda/inst/java

import cc.mallet.types.*;
import cc.mallet.topics.*;

public class BigTopicModel extends RTopicModel {
	
	public BigTopicModel(double numTopics, double alpha, double beta) {
		super(numTopics, alpha, beta);
	}

	public int[] getDocLengthCounts() {
		
		int[] docLengthCounts = new int[ data.size() ];
		
		for (int doc = 0; doc < data.size(); doc++) {
        
        FeatureSequence fs = (FeatureSequence) data.get(doc).instance.getData();
        docLengthCounts[doc] = fs.getLength();
    
		}
		
		return docLengthCounts;
	}
}

