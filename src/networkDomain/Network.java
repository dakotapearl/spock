package networkDomain;

import java.util.ArrayList;

import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class Network extends Thread {
	
	public ArrayList<NetworkNode> nodes;
	
	public Network() {
		nodes = new ArrayList<NetworkNode>();
	}
	
	public void start() {
		Assert.AssertTrue("Starting network of positive size", networkSize() > 0);
		
		Log.writeForThreadCreation("Network");
		Assert.CriticalAssertTrue("Newtork thread started", this.isAlive());
		
		for (NetworkNode n : nodes)
			n.start();
	}
	
	public long networkSize() {
		return nodes.size();
	}
	
}
