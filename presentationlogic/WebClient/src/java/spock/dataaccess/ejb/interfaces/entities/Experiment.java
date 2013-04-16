package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author loren
 */
public interface Experiment extends Serializable {

    void addEnvironment(Environment environment);
    void addNetwork(Network network);
    void addUserInterface(UserInterface userInterface);
    Collection<Environment> getEnvironments();
    String getId();
    Collection<UserInterface> getInterfaces();
    Boolean getIsActive();
    Collection<Network> getNetworks();
    void removeEnvironment(Environment environment);
    void removeNetwork(Network network);
    void removeUserInterface(UserInterface userInterface);
    void setId(String id);
    void setIsActive(Boolean IsActive);
    
}
