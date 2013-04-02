package spock.network.behaviours;

import java.util.HashMap;
import spock.network.NetworkNode;

/**
 * @author Loren Chorley
 */
public class NodeProperties implements NetworkBehaviour {
	
	protected NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") NodeProperties newFunction;
		public replicator(NodeProperties newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(NodeProperties newFunction) { (new replicator(newFunction)).start(); }
	public NodeProperties replicate() {
		//TODO
		return null;
	}

	@SuppressWarnings({ "rawtypes", "unused" })
	protected class GSProperty {
		public String id;
		public Class datatype;
		public Object value;
		public GeneFunctions fns;
	}
	protected HashMap<String, GSProperty> sequence;
	
	public NodeProperties() {
		sequence = new HashMap<String, GSProperty>();
		
		requestProperty("Mutation rate", Double.class, 0.0, new GeneFunctions() {
			
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
	public void requestProperty(String id, Class datatype, Object initialValue, GeneFunctions functions) {
		assert(id != null && datatype != null && initialValue != null && functions != null) : "requestGene parameters all non-null";
		
		GSProperty p = new GSProperty();
		p.id = id;
		p.datatype = datatype;
		p.value = initialValue;
		p.fns = functions;
		sequence.put(id, p);
		
		assert(p.fns.compare(p.value, p.fns.getLowerBound()) >= 0 &&
                       p.fns.compare(p.value, p.fns.getUpperBound()) <= 0) : "New gene value (" + p.value 
				                  + ") is within bounds [" + p.fns.getLowerBound()
				                  + ", " + p.fns.getUpperBound() + "]";
	}
	
	public void setProperty(String id, Object value) {
		assert(sequence.containsKey(id)) : "id not found";
		
		sequence.get(id).value = value;
		
		assert(sequence.get(id).fns.compare(sequence.get(id).value, sequence.get(id).fns.getLowerBound()) >= 0 &&
                       sequence.get(id).fns.compare(sequence.get(id).value, sequence.get(id).fns.getUpperBound()) <= 0) : "Gene value is within bounds";
	}
	
	public Object getProperty(String id) {
		assert(sequence.containsKey(id)) : "id not found";
		
		return sequence.get(id).value;
	}
		
}
