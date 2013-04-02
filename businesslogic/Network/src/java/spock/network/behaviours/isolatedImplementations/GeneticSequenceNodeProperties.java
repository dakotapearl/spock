package spock.network.behaviours.isolatedImplementations;

import java.util.Random;
import spock.network.behaviours.NodeProperties;

/**
 * @author Loren Chorley
 */
public class GeneticSequenceNodeProperties extends NodeProperties {
	
	protected GeneticSequenceNodeProperties mutate(double mutationRate) {
		Random r = new Random();
		try {
			GeneticSequenceNodeProperties gs = this.getClass().newInstance();
			for (String id : sequence.keySet()) {
				if (r.nextDouble() <= mutationRate)
					gs.requestProperty(id, sequence.get(id).datatype, sequence.get(id).fns.mutate(sequence.get(id).value, mutationRate), sequence.get(id).fns);
				else
					gs.requestProperty(id, sequence.get(id).datatype, sequence.get(id).value, sequence.get(id).fns);
				// TODO add something that implements the mutation rate
			}
			return gs;
		} catch (InstantiationException e) {
			e.printStackTrace();
			System.exit(-1);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			System.exit(-1);
		}
		return null;
	}
	
	protected GeneticSequenceNodeProperties splice(GeneticSequenceNodeProperties geneticSequence) {
		// TODO
		return null;
	}
	
}
