package networkDomain.core;

import networkDomain.NetworkNode;
import networkDomain.NetworkSignal;

public class OutputProcess extends Thread {
	
	NetworkNode parent;
	
	public void declareParent(NetworkNode parent) { this.parent = parent; }
	
	@Override
	public void run() {
		NetworkSignal signal;
		
		while (true) {
			
			
			
			// Select signal to fire
			signal = parent.transmissionContent.nextSignalToFire();
			
			
			
		}
		
	}
	
}
