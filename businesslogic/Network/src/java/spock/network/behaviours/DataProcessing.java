package spock.network.behaviours;

import java.io.Serializable;
import spock.network.core.NetworkNode;
import spock.network.signals.NetworkSignal;

/**
 * @author Loren Chorley
 */
public abstract class DataProcessing extends NetworkBehaviour<DataProcessing> {

    @Override
    public void replaceInNode(NetworkNode node, DataProcessing behaviour) {
        node.dataProcessing = behaviour;
    }
    
    public abstract NetworkSignal processData(NetworkSignal data); // Make sure that it's a different object that gets returned

}
