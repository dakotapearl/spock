package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface EnvironmentNode extends Serializable {

    Environment getEnvironment();
    EnvironmentInterface getEnvironmentInterface();
    Long getId();
    void setEnvironment(Environment environment);
    void setEnvironmentInterface(EnvironmentInterface environmentInterface);
    void setId(Long id);
    
}
