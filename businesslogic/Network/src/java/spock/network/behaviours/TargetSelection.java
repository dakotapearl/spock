package spock.network.behaviours;

import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;

/**
 * @author Loren Chorley
 */
public abstract class TargetSelection extends NetworkBehaviour<TargetSelection> {
    
    @Override
    public void replaceInNode(NetworkNode node, TargetSelection behaviour) {
        node.targetSelection = behaviour;
    }
    
    public abstract NetworkNode selectTarget(NetworkSignal signal);

}
