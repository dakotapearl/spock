package spock.common.interfaces;

import java.util.List;
import spock.common.exceptions.*;


/**
 *
 * @author Loren Chorley
 */
public interface EnvNetInterface<N> {
    
    /**
     * 
     * @return 
     */
    public int getNumberOfNodes();
    
    /**
     * 
     * @return 
     */
    public List<N> getInterfaceNodes();
    
    /**
     * 
     * @return 
     */
    public boolean isInputInterface();
    
    /**
     * 
     * @param enInterface
     * @throws InterfaceNotCompatibleException
     * @throws InterfaceAlreadyConnectedException 
     */
    public void acceptConnection(EnvNetInterface enInterface) throws InterfaceNotCompatibleException, InterfaceAlreadyConnectedException;
    
    /**
     * 
     */
    public void disconnect();
    
}
