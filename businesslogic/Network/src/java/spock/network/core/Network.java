package spock.network.core;

import java.util.ArrayList;
import java.util.HashMap;

import spock.network.behaviours.NetworkRunnable;
import tools.errorChecking.Assert;
import tools.errorChecking.Log;

/**
 * @author Loren Chorley
 */
public class Network extends Thread implements NetworkRunnable {
	
    public ArrayList<NetworkNode> nodes;
    public HashMap<String, NetworkObservable> observables;

    /*public void nodeactivation() {
            observables.get("Node activations").updateInterface(Integer.toString(++c));
    }*/

    public Network() {

        // Initialise primary network objects
        nodes = new ArrayList<NetworkNode>();
        observables = new HashMap<String, NetworkObservable>();

        // Initialise interface variables
        observables.put("Number of nodes", new NetworkObservable("Number of nodes"));
        //observables.put("Node activations", new NetworkObservable("Node activations"));


    }

    @Override
    public void run() {
        assert(networkSize() > 0) : "Starting network of positive size";

        Log.writeForThreadCreation("Network");
        assert(this.isAlive()) : "Newtork thread started";

        for (NetworkNode n : nodes)
            n.start();

        //observables.get("Number of nodes").updateInterface(Integer.toString(nodes.size()));

    }

    public long networkSize() {
        return nodes.size();
    }

    public long getUniqueNodeID() {
        return 0;
    }
    
    @Override
    public void pauseActivity() {
        for (NetworkNode n : nodes)
            n.pauseActivity();
    }

    @Override
    public void resumeActivity() {
        for (NetworkNode n : nodes)
            n.resumeActivity();
    }

}
