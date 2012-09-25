package networkDomain;

import java.util.ArrayList;

import tools.errorChecking.Assert;

public class Network {
	
	public ArrayList<NetworkNode> nodes;
	
	public Network() {
		nodes = new ArrayList<NetworkNode>();
	}
	
	public void start() {
		Assert.AssertTrue("Starting network of positive size", networkSize() > 0);
		
		for (NetworkNode n : nodes)
			n.start();
	}
	
	public long networkSize() {
		return nodes.size();
	}
	
}
