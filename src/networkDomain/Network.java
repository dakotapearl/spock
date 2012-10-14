package networkDomain;

import interfaceDomain.InterfaceObservable;

import java.util.ArrayList;
import java.util.HashMap;

import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class Network extends Thread {
	
	public NetworkDomain networkDomain;
	public ArrayList<NetworkNode> nodes;
	public HashMap<String, InterfaceObservable> interfaceObservables;
	
	int c=0;
	
	public void nodeactivation() {
		interfaceObservables.get("Node activations").updateInterface(Integer.toString(++c));
	}
	
	public Network(NetworkDomain networkDomain) {
		this.networkDomain = networkDomain;
		
		nodes = new ArrayList<NetworkNode>();
		interfaceObservables = new HashMap<String, InterfaceObservable>();
		
		// Initialise interface variables
		interfaceObservables.put("Number of nodes", new InterfaceObservable("Number of nodes"));
		networkDomain.interfaceDomain.registerInterfaceObservable(interfaceObservables.get("Number of nodes"));
		interfaceObservables.put("Node activations", new InterfaceObservable("Node activations"));
		networkDomain.interfaceDomain.registerInterfaceObservable(interfaceObservables.get("Node activations"));
	}
	
	public void run() {
		Assert.AssertTrue("Starting network of positive size", networkSize() > 0);
		
		Log.writeForThreadCreation("Network");
		Assert.CriticalAssertTrue("Newtork thread started", this.isAlive());
		
		for (NetworkNode n : nodes)
			n.start();
		
		interfaceObservables.get("Number of nodes").updateInterface(Integer.toString(nodes.size()));
		
	}
	
	public long networkSize() {
		return nodes.size();
	}
	
	public void pauseNetwork() {
		for (NetworkNode n : nodes)
			n.pauseNode();
	}
	
	public void resumeNetwork() {
		for (NetworkNode n : nodes)
			n.resumeNode();
	}
	
}
