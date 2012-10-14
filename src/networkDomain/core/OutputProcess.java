package networkDomain.core;

import tools.errorChecking.Log;
import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;
import networkDomain.NetworkTargetable;

/**
 * @author Loren Chorley
 */
public class OutputProcess extends Thread {
	
	NetworkNode parent;
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	private class replicator extends Thread {
		@SuppressWarnings("unused") OutputProcess newFunction;
		public replicator(OutputProcess newFunction) { this.newFunction = newFunction; }
		@Override public void run() { newFunction = replicate(); }
	}
	public void replicateFunction(OutputProcess newFunction) { (new replicator(newFunction)).start(); }
	public OutputProcess replicate() {
		OutputProcess n;
		try {
			n = this.getClass().newInstance();
			return n; 
		} catch (InstantiationException e) {
			e.printStackTrace();
			System.exit(1);
		} catch (IllegalAccessException e) {
			e.printStackTrace();
			System.exit(1);
		} 
		return null;
	}
	
	@Override
	public void run() {
		NetworkSignal signal;
		NetworkTargetable target;
		
		while (true) {
			
			Log.writeForMechanisms("OutputProcess: waiting for ready to fire");
			
			// wait until ready to fire
			if (parent.firingCondition.waitUntilReadyToFire()) {
				
				Log.writeForMechanisms("OutputProcess: Firing Condition set");
				
				// Select signal to fire
				signal = parent.transmissionContent.nextSignalToFire();
				Log.writeForMechanisms("OutputProcess: Selected signal");
				
				// Select target
				target = parent.targetSelection.selectTarget(signal);
				Log.writeForMechanisms("OutputProcess: Selected target");
				
				// Fire
				Log.write("Node firing signal with datum: " + signal.getData().getDatum().getValue().toString());
				target.acceptSignal(signal, parent);
				
				Log.writeForMechanisms("OutputProcess: Sent signal");
				
				parent.networkDomain.getNetwork().nodeactivation();
				parent.networkDomain.getNetwork().interfaceObservables.get("Latest transfer").updateInterface(Integer.toString(parent.getID()) + " -> " + Integer.toString(target.getID()));
				
			}
			
		}
		
	}
	
}
