package networkDomain.extensions;

import java.util.HashMap;
import tools.errorChecking.Assert;
import networkDomain.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class GeneticSequence implements Runnable {
	
	public Thread thread = new Thread(this);
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") GeneticSequence newFunction;
		public replicator(GeneticSequence newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(GeneticSequence newFunction) { (new replicator(newFunction)).start(); }
	public abstract GeneticSequence replicate(); //Can call either of mutate or splice, or do something else
	
	@SuppressWarnings({ "rawtypes", "unused" })
	private class GSProperty {
		String id;
		Class datatype;
		Object value;
	}
	private HashMap<String, GSProperty> sequence;
	
	public GeneticSequence() {
		sequence = new HashMap<String, GSProperty>();
	}
	
	@SuppressWarnings("rawtypes")
	public void requestProperty(String id, Class datatype, Object initialValue) {
		GSProperty p = new GSProperty();
		p.id = id;
		p.datatype = datatype;
		p.value = initialValue;
		sequence.put(id, p);
	}
	
	public void setProperty(String id, Object value) {
		Assert.CriticalAssertTrue("id not found", sequence.containsKey(id));
		
		sequence.get(id).value = value;
	}
	
	public Object getProperty(String id) {
		Assert.CriticalAssertTrue("id not found", sequence.containsKey(id));
		
		return sequence.get(id).value;
	}
	
	protected GeneticSequence mutate(double mutationRate) {
		try {
			GeneticSequence gs = this.getClass().newInstance();
			for (String id : sequence.keySet()) {
				gs.requestProperty(id, sequence.get(id).datatype, sequence.get(id).value);
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
