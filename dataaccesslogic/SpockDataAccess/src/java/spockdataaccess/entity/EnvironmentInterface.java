/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author loren
 */
@Entity
@Table(name = "EnvironmentInterfaces")
public class EnvironmentInterface implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected Integer NumberOfNodes;
    @OneToMany(mappedBy="environmentInterface")
    protected Collection<EnvironmentNode> environmentNodes;
    @NotNull
    @ManyToOne
    protected Environment environment;
    @NotNull
    protected Boolean isInputInterface;
    
    public EnvironmentInterface() {
        environmentNodes = new ArrayList<EnvironmentNode>();
    }
    
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getNumberOfNodes() {
        return NumberOfNodes;
    }

    public void setNumberOfNodes(Integer NumberOfNodes) {
        this.NumberOfNodes = NumberOfNodes;
    }

    public Collection<EnvironmentNode> getEnvironmentNodes() {
        return environmentNodes;
    }

    public void setEnvironmentNodes(Collection<EnvironmentNode> environmentNodes) {
        this.environmentNodes = environmentNodes;
    }

    public void addEnvironmentNode(EnvironmentNode environmentNode) {
        environmentNodes.add(environmentNode);
    }

    public Boolean getIsInputInterface() {
        return isInputInterface;
    }

    public void setIsInputInterface(Boolean isInputInterface) {
        this.isInputInterface = isInputInterface;
    }

    public Environment getEnvironment() {
        return environment;
    }

    public void setEnvironment(Environment environment) {
        this.environment = environment;
    }
    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof EnvironmentInterface)) {
            return false;
        }
        EnvironmentInterface other = (EnvironmentInterface) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.EnvironmentInterface[ id=" + id + " ]";
    }
    
}
