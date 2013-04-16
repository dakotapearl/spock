package spock.network.behaviours;

import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;

/**
 * @author Loren Chorley
 */
public abstract class TransmissionContent extends NetworkBehaviour<TransmissionContent> {

    @Override
    public void replaceInNode(NetworkNode node, TransmissionContent behaviour) {
        node.transmissionContent = behaviour;
    }
    
    public abstract NetworkSignal selectContent();
    public abstract boolean signalsRemain();

}
