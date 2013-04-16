package spock.network.behaviours.independent;

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
			for (String id : properties.keySet()) {
				if (r.nextDouble() <= mutationRate)
					gs.requestProperty(id, properties.get(id).datatype, properties.get(id).fns.mutate(properties.get(id).value, mutationRate), properties.get(id).fns);
				else
					gs.requestProperty(id, properties.get(id).datatype, properties.get(id).value, properties.get(id).fns);
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
