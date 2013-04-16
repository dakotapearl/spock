package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface InterfaceConnection extends Serializable {

    EnvironmentInterface getEnvironmentInterface();
    Experiment getExperiment();
    Long getId();
    NetworkInterface getNetworkInterface();
    void setEnvironmentInterface(EnvironmentInterface environmentInterface);
    void setExperiment(Experiment experiment);
    void setId(Long id);
    void setNetworkInterface(NetworkInterface networkInterface);
    
}
