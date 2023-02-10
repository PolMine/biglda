// javac -cp /opt/mallet/Mallet-202108/class BigTopicModel.java
// then
// mv BigTopicModel.class ~/Lab/github/biglda/inst/java/

import cc.mallet.types.*;
import cc.mallet.topics.*;
import java.io.IOException;
import java.io.PrintWriter;

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
	
	public void printNonzeroTopicWordWeights(PrintWriter out) throws IOException {

      for (int topic = 0; topic < numTopics; topic++) {
          for (int type = 0; type < numTypes; type++) {

              int[] topicCounts = typeTopicCounts[type];

              double weight = beta;

              int index = 0;
              while (index < topicCounts.length && topicCounts[index] > 0) {

                  int currentTopic = topicCounts[index] & topicMask;

                  if (currentTopic == topic) {
                      weight += topicCounts[index] >> topicBits;
                      break;
                  }

                  index++;
              }
              
              if (weight > beta){
                  out.println(topic + "\t" + alphabet.lookupObject(type) + "\t" + weight);
              }

          }
      }
  }
}

