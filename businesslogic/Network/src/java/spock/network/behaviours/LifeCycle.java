package spock.network.behaviours;

import spock.network.core.NetworkNode;

/**
 * @author Loren Chorley
 */
public abstract class LifeCycle extends NetworkBehaviour<LifeCycle> {

    @Override
    public void replaceInNode(NetworkNode node, LifeCycle behaviour) {
        node.lifeCycle = behaviour;
    }
    
    // cell death condition
    // neurogenic mitosis condition

}
