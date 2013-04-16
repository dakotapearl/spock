package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author loren
 */
public interface EnvironmentInterface extends Serializable {

    void addEnvironmentNode(EnvironmentNode environmentNode);
    Environment getEnvironment();
    Collection<EnvironmentNode> getEnvironmentNodes();
    Long getId();
    Boolean getIsInputInterface();
    Integer getNumberOfNodes();
    void setEnvironment(Environment environment);
    void setEnvironmentNodes(Collection<EnvironmentNode> environmentNodes);
    void setId(Long id);
    void setIsInputInterface(Boolean isInputInterface);
    void setNumberOfNodes(Integer NumberOfNodes);
    
}
