package spock.network.core;

import java.util.List;
import spock.common.exceptions.InterfaceAlreadyConnectedException;
import spock.common.exceptions.InterfaceNotCompatibleException;
import spock.common.interfaces.EnvNetInterface;

public class NetworkInterface implements EnvNetInterface {
    
    private List<NetworkNode> networkNodes;
    private Network network; //May not be necessary
    private boolean isInputInterface;
    private int NumberOfNodes;
    private EnvNetInterface enInterface;

    public NetworkInterface(List<NetworkNode> networkNodes, Network network, boolean isInputInterface, int NumberOfNodes) {
        this.networkNodes = networkNodes;
        this.network = network;
        this.isInputInterface = isInputInterface;
        this.NumberOfNodes = NumberOfNodes;
    }

    @Override
    public int getNumberOfNodes() {
        return NumberOfNodes;
    }

    @Override
    public boolean isInputInterface() {
        return isInputInterface;
    }

    @Override
    public List<NetworkNode> getInterfaceNodes() {
        return networkNodes;
    }
    
    /**
     * 
     * @return 
     */
    public boolean isConnected() {
        return false;
    }
    
    /**
     * Check the number of nodes plus input output types
     * @param enInterface
     * @return 
     */
    public boolean isCompatible(EnvNetInterface enInterface) {
        return (NumberOfNodes == enInterface.getNumberOfNodes()) 
            && (isInputInterface != enInterface.isInputInterface());
    }
    
    @Override
    public void acceptConnection(EnvNetInterface enInterface) throws InterfaceNotCompatibleException, InterfaceAlreadyConnectedException {
        
        // Check if this interface is already connected to another interface
        if (isConnected()) {
            throw new InterfaceAlreadyConnectedException();
        }
        
        // Check if the passed interface is compatible with this one
        if (!isCompatible(enInterface)) {
            throw new InterfaceNotCompatibleException();
        }
        
        // Now responsible for connecting the network nodes to the other interface's nodes, but not the other way around.
        // But only if it is an output interface, otherwise it does nothing.
        List remoteNodes = enInterface.getInterfaceNodes();
        
        // remoteNodes.size() = networkNodes.size() because of isCompatible check
        if (!isInputInterface) {
            for (int i = 0; i < remoteNodes.size(); i++) {

                //remoteNodes.get(i)
                //networkNodes.get(i).targetSelection.
            }
        }
        
    }
    
    @Override
    public void disconnect() {
        
    }
    
}
