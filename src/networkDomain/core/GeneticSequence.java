package networkDomain.core;

import java.util.HashMap;
import java.util.Random;

import tools.errorChecking.Assert;
import networkDomain.NetworkBehaviour;
import networkDomain.NetworkNode;

/**
 * @author Loren Chorley
 */
public class GeneticSequence implements NetworkBehaviour {
	
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") GeneticSequence newFunction;
		public replicator(GeneticSequence newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(GeneticSequence newFunction) { (new replicator(newFunction)).start(); }
	public GeneticSequence replicate() {
		//TODO
		return null;
	}

	@SuppressWarnings({ "rawtypes", "unused" })
	private class GSProperty {
		public String id;
		public Class datatype;
		public Object value;
		public GeneFunctions fns;
	}
	private HashMap<String, GSProperty> sequence;
	
	public GeneticSequence() {
		sequence = new HashMap<String, GSProperty>();
		
		requestGene("Mutation rate", Double.class, 0.0, new GeneFunctions() {
			
			@Override
			public Object mutate(Object originalValue, double mutationRate) {
				return originalValue;
			}
			
			@Override
			public Object getUpperBound() {
				return 1.0;
			}
			
			@Override
			public Object getLowerBound() {
				return 0.0;
			}

			@Override
			public int compare(Object thisValue, Object otherValue) {
				if (Double.parseDouble(thisValue.toString()) == Double.parseDouble(otherValue.toString()))
					return 0;
				else
					return Double.parseDouble(thisValue.toString()) > Double.parseDouble(otherValue.toString()) ? 1 : -1;
			}
			
		});
		
	}
	
	@SuppressWarnings("rawtypes")
	public void requestGene(String id, Class datatype, Object initialValue, GeneFunctions functions) {
		Assert.CriticalAssertTrue("requestGene parameters all non-null", id != null && 
				                                                         datatype != null && 
				                                                         initialValue != null && 
				                                                         functions != null);
		
		GSProperty p = new GSProperty();
		p.id = id;
		p.datatype = datatype;
		p.value = initialValue;
		p.fns = functions;
		sequence.put(id, p);
		
		Assert.CriticalAssertTrue("New gene value (" + p.value 
				                  + ") is within bounds [" + p.fns.getLowerBound()
				                  + ", " + p.fns.getUpperBound() + "]", 
				                  p.fns.compare(p.value, p.fns.getLowerBound()) >= 0 &&
								  p.fns.compare(p.value, p.fns.getUpperBound()) <= 0);
	}
	
	public void setGene(String id, Object value) {
		Assert.CriticalAssertTrue("id not found", sequence.containsKey(id));
		
		sequence.get(id).value = value;
		
		Assert.CriticalAssertTrue("Gene value is within bounds", sequence.get(id).fns.compare(sequence.get(id).value, sequence.get(id).fns.getLowerBound()) >= 0 &&
				                                                 sequence.get(id).fns.compare(sequence.get(id).value, sequence.get(id).fns.getUpperBound()) <= 0);
	}
	
	public Object getGene(String id) {
		Assert.CriticalAssertTrue("id not found", sequence.containsKey(id));
		
		return sequence.get(id).value;
	}
	
	protected GeneticSequence mutate(double mutationRate) {
		Random r = new Random();
		try {
			GeneticSequence gs = this.getClass().newInstance();
			for (String id : sequence.keySet()) {
				if (r.nextDouble() <= mutationRate)
					gs.requestGene(id, sequence.get(id).datatype, sequence.get(id).fns.mutate(sequence.get(id).value, mutationRate), sequence.get(id).fns);
				else
					gs.requestGene(id, sequence.get(id).datatype, sequence.get(id).value, sequence.get(id).fns);
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
	
	protected GeneticSequence splice(GeneticSequence geneticSequence) {
		// TODO
		return null;
	}
	
}
